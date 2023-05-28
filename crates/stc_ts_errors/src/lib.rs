#![allow(incomplete_features)]
#![deny(variant_size_differences)]
#![feature(box_syntax)]
#![feature(specialization)]
#![cfg_attr(not(debug_assertions), allow(unused))]

use std::{
    fmt,
    fmt::{Debug, Display},
    ops::RangeInclusive,
    panic::Location,
    path::PathBuf,
};

use ansi_term::Color::Yellow;
use derivative::Derivative;
use fmt::Formatter;
use scoped_tls::scoped_thread_local;
use static_assertions::assert_eq_size;
use stc_ts_types::{name::Name, Id, Key, ModuleId, Type, TypeElement, TypeParamInstantiation};
use stc_utils::stack::StackOverflowError;
use swc_atoms::JsWord;
use swc_common::{
    errors::{DiagnosticId, Handler},
    Span, Spanned, DUMMY_SP,
};
use swc_ecma_ast::{AssignOp, BinaryOp, UpdateOp};

pub use self::result_ext::DebugExt;

pub mod debug;
mod result_ext;

scoped_thread_local!(pub static DISABLE_ERROR_CONTEXT: ());

/// [ErrorKind] with debug contexts attached.
#[derive(Clone, PartialEq, Spanned)]
pub struct Error {
    #[cfg(debug_assertions)]
    contexts: Vec<String>,
    #[span]
    inner: Box<ErrorKind>,
}

impl std::ops::Deref for Error {
    type Target = ErrorKind;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl From<ErrorKind> for Error {
    fn from(kind: ErrorKind) -> Self {
        Self {
            #[cfg(debug_assertions)]
            contexts: Default::default(),
            inner: Box::new(kind),
        }
    }
}

impl Error {
    #[track_caller]
    pub fn context(self, context: impl Display) -> Error {
        return self.context_impl(Location::caller(), context);
    }

    #[cfg_attr(not(debug_assertions), inline(always))]
    pub(crate) fn context_impl(mut self, loc: &'static Location, context: impl Display) -> Error {
        #[cfg(debug_assertions)]
        {
            if DISABLE_ERROR_CONTEXT.is_set() {
                return self;
            }

            self.contexts
                .push(format!("{} (at {}:{}:{})", context, loc.file(), loc.line(), loc.column()));
        }
        self
    }

    #[cold]
    pub fn emit(&self, h: &Handler) {
        let span = self.span();

        let mut err = h.struct_span_err_with_code(
            span,
            &format!("{:#?}", self),
            DiagnosticId::Error(format!("TS{}", ErrorKind::normalize_error_code(self.code()))),
        );

        err.emit();
    }
}

impl Debug for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        #[cfg(debug_assertions)]
        for ctx in self.contexts.iter().rev() {
            writeln!(f, "{}: {}", Yellow.paint("context"), ctx)?;
        }

        Debug::fmt(&self.inner, f)?;

        Ok(())
    }
}

impl Errors {
    /// This is used for debugging (by calling [panic]).
    #[allow(clippy::only_used_in_recursion)]
    fn validate(&self, err: &Error) {
        if let Ok(var) = std::env::var("DBG_ERROR") {
            let s = format!("{:?}", err);
            if !var.is_empty() && s.contains(&var) {
                crate::debug::print_backtrace();
            }
        }

        if let ErrorKind::Errors { ref errors, .. } = &*err.inner {
            for err in errors {
                self.validate(err)
            }
            return;
        }

        let code = err.code();
        if (5000..6000).contains(&code) {
            // This is error for invalid options.
        } else if err.span().is_dummy() {
            unreachable!("Error with a dummy span found: {:?}", err)
        }
    }
}

#[derive(Derivative, Clone, PartialEq, Spanned)]
#[derivative(Debug)]
pub enum ErrorKind {
    /// TS2559
    NoCommonProperty {
        span: Span,
    },
    /// TS7026
    ImplicitAnyBecauseThereIsNoJsxInterface {
        span: Span,
    },

    /// TS2538
    TypeCannotBeUsedForIndex {
        span: Span,
        prop: Box<Key>,
    },

    /// TS2698
    NonObjectInSpread {
        span: Span,
        ty: Box<Type>,
    },

    /// TS2312
    NotExtendableType {
        span: Span,
    },

    /// TS2430
    InvalidInterfaceInheritance {
        span: Span,
        cause: Box<Error>,
    },

    /// TS2339
    TupleTooShort {
        span: Span,
    },

    /// TS2403
    VarDeclNotCompatible {
        span: Span,

        cause: Box<Error>,
    },

    /// TS2795
    IntrinsicIsBuiltinOnly {
        span: Span,
    },

    /// TS2347
    TypeParamsProvidedButCalleeIsNotGeneric {
        span: Span,
    },

    /// TS2386
    OptionalAndNonOptionalMethodPropertyMixed {
        span: Span,
    },

    /// TS2357
    UpdateArgMustBeVariableOrPropertyAccess {
        span: Span,
    },

    /// TS2320
    InterfaceNotCompatible {
        span: Span,
    },

    /// TS2538
    CannotUseTypeAsIndexIndex {
        span: Span,
    },

    /// TS2534
    /// A function returning 'never' cannot have a reachable end point.
    CannotFunctionReturningNever {
        span: Span,
    },

    /// TS2432
    OnlyOneEnumCanOmitInit {
        span: Span,
    },

    /// TS2477
    ConstEnumMemberHasInfinityAsInit {
        span: Span,
    },

    /// TS2478
    ConstEnumMemberHasNaNAsInit {
        span: Span,
    },

    /// TS7027
    UnreachableCode {
        span: Span,
    },

    /// TS2454
    VarMayNotBeInitialized {
        span: Span,
    },

    /// TS2564
    ClassPropNotInitialized {
        span: Span,
    },

    /// TS2610
    DefinedWithAccessorInSuper {
        span: Span,
    },
    /// TS5048
    OptionInvalidForEs3 {
        span: Span,
    },

    /// TS17009
    ThisUsedBeforeCallingSuper {
        span: Span,
    },

    /// TS17011
    SuperUsedBeforeCallingSuper {
        span: Span,
    },

    /// TS2337
    SuperInNestedFunction {
        span: Span,
    },

    /// TS2377
    SuperNotCalled {
        span: Span,
    },

    /// TS2513
    CannotAccessAbstractMember {
        span: Span,
    },

    /// TS2365
    OperatorCannotBeAppliedToTypes {
        span: Span,
    },

    /// TS2516
    AbstractClassMethodShouldBeSequential {
        span: Span,
    },

    /// TS2411
    ClassMemberNotCompatibleWithStringIndexSignature {
        span: Span,
    },

    /// TS2411
    ClassMemberNotCompatibleWithNumericIndexSignature {
        span: Span,
    },

    /// TS2322
    AssignFailedBecauseTupleLengthDiffers {
        span: Span,
    },

    /// TS17013
    InvalidUsageOfNewTarget {
        span: Span,
    },

    /// TS2767
    ReturnPropertyOfIteratorMustBeMethod {
        span: Span,
    },

    /// TS2490
    NextOfIteratorShouldReturnTypeWithPropertyValue {
        span: Span,
    },

    /// TS2631
    CannotAssignToNamespace {
        span: Span,
    },

    /// TS2701
    RestArgMustBeVarOrMemberAccess {
        span: Span,
    },

    /// TS2777
    InvalidOperandOfIncDecOptionalProp {
        span: Span,
    },

    /// TS2778
    InvalidRestPatternInOptionalChain {
        span: Span,
    },

    /// TS2779
    InvalidLhsOfAssignOptionalProp {
        span: Span,
    },

    /// TS2780
    InvalidRestPatternInForIn {
        span: Span,
    },

    /// TS2781
    InvalidRestPatternInForOf {
        span: Span,
    },

    /// TS2501
    BindingPatNotAllowedInRestPatArg {
        span: Span,
    },

    /// TS2790
    DeleteOperandMustBeOptional {
        span: Span,
    },

    /// TS2754
    SuperCannotUseTypeArgs {
        span: Span,
    },

    /// TS7009
    TargetLacksConstructSignature {
        span: Span,
    },

    /// TS2448
    BlockScopedVarUsedBeforeInit {
        span: Span,
    },

    /// TS2528
    DuplicateDefaultExport {
        span: Span,
    },

    /// TS2323
    DuplicateExport {
        span: Span,
    },

    /// TS2393
    DuplicateFnImpl {
        span: Span,
    },

    /// TS2392
    DuplicateConstructor {
        span: Span,
    },

    /// TS2307
    ModuleNotFound {
        span: Span,
    },

    /// TS5061
    TooManyAsterisk {
        span: Span,
    },

    /// TS2451
    DuplicateVar {
        name: Id,
        span: Span,
    },

    /// TS2725
    ClassNameCannotBeObjectWhenTargetingEs5WithModule {
        span: Span,
    },

    /// TS2461
    NotArrayType {
        span: Span,
    },

    /// TS2495
    NotArrayTypeNorStringType {
        span: Span,
    },

    /// TS2569
    NotArrayTypeNorStringTypeButDownlevelIterationWouldWork {
        span: Span,
    },

    /// TS2494
    ForOfStringUsedInEs3 {
        span: Span,
    },

    /// TS2480
    LetOrConstIsNotValidIdInLetOrConstVarDecls {
        span: Span,
    },

    /// TS2406
    InvalidExprOfLhsOfForIn {
        span: Span,
    },

    /// TS2487
    InvalidExprOfLhsOfForOf {
        span: Span,
    },

    /// TS2405
    WrongTypeForLhsOfForInLoop {
        span: Span,
    },

    /// TS2491
    DestructuringBindingNotAllowedInLhsOfForIn {
        span: Span,
    },

    /// TS2404
    TypeAnnOnLhsOfForInLoops {
        span: Span,
    },

    /// TS2483
    TypeAnnOnLhsOfForOfLoops {
        span: Span,
    },

    /// TS18013
    CannotAccessPrivatePropertyFromOutside {
        span: Span,
    },

    /// TS18030
    OptionalChainCannotContainPrivateIdentifier {
        span: Span,
    },

    /// TS18011
    CannotDeletePrivateProperty {
        span: Span,
    },

    /// TS18012
    ConstructorIsKeyword {
        span: Span,
    },

    /// TS18022
    PrivateIdUsedAsMethodName {
        span: Span,
    },

    /// TS18050
    UndefinedOrNullIsNotValidOperand {
        span: Span,
    },

    /// TS2333
    ThisInConstructorParam {
        span: Span,
    },

    /// TS2334
    ThisInStaticPropertyInitializer {
        span: Span,
    },

    /// TS2507
    InvalidSuperClass {
        span: Span,
    },

    /// TS2410
    WithStmtNotSupported {
        span: Span,
    },

    /// TS2503
    NamespaceNotFound {
        name: Box<Name>,
        ctxt: ModuleId,
        type_args: Option<Box<TypeParamInstantiation>>,
        span: Span,
    },

    /// TS2452
    EnumMemberIdCannotBeNumber {
        span: Span,
    },

    /// TS2364
    InvalidLhsOfAssign {
        span: Span,
    },

    /// TS7010
    ImplicitReturnType {
        span: Span,
    },

    /// TS2394
    IncompatibleFnOverload {
        span: Span,
        cause: Box<Error>,
    },

    /// TS2371
    InitializerDisallowedInAmbientContext {
        span: Span,
    },

    /// TS2414
    InvalidClassName {
        span: Span,
    },

    /// TS18004
    NoSuchVarForShorthand {
        span: Span,
        name: Id,
    },

    /// TS2769
    NoMatchingOverload {
        span: Span,
    },

    /// TS2427
    InvalidInterfaceName {
        span: Span,
    },

    // TS2350
    CannotCallWithNewNonVoidFunction {
        span: Span,
    },

    /// TS2300
    DuplicateProperty {
        span: Span,
    },

    /// TS2661
    CannotExportNonLocalVar {
        span: Span,
    },

    /// TS2699
    StaticPropertyCannotBeNamedPrototype {
        span: Span,
    },

    /// TS2506
    SelfReferentialSuperClass {
        span: Span,
    },

    /// TS2507
    NotConstructorType {
        span: Span,
    },

    /// TS2395
    ExportMixedWithLocal {
        span: Span,
    },

    /// TS2652
    MixedDefaultExports {
        span: Span,
    },

    /// TS2420
    ClassIncorrectlyImplementsInterface {
        span: Span,
    },

    StackOverflow {
        span: Span,
    },

    /// TS2420
    InvalidImplOfInterface {
        span: Span,
        cause: Box<Error>,
    },

    /// TS2302
    StaticMemberCannotUseTypeParamOfClass {
        span: Span,
    },

    /// TS2467
    DeclaringTypeParamReferencedByComputedPropName {
        span: Span,
    },

    /// TS2465
    CannotReferenceThisInComputedPropName {
        span: Span,
    },

    /// TS2466
    CannotReferenceSuperInComputedPropName {
        span: Span,
    },

    /// TS2331
    ThisRefToModuleOrNamespace {
        span: Span,
    },

    /// TS2335
    SuperInClassWithoutSuper {
        span: Span,
    },

    /// TS2660
    SuperCanBeOnlyReferencedInDerivedClass {
        span: Span,
    },

    /// TS2505
    GeneratorCannotHaveVoidAsReturnType {
        span: Span,
    },

    /// TS2663
    NoSuchVarButThisHasSuchProperty {
        span: Span,
        name: Id,
    },

    DestructuringAssignInAmbientContext {
        span: Span,
    },

    /// TS2463
    OptionalBindingPatternInImplSignature {
        span: Span,
    },
    /// TS5076
    NullishCoalescingMixedWithLogicalWithoutParen {
        span: Span,
    },

    /// TS2678
    SwitchCaseTestNotCompatible {
        span: Span,
        disc: Box<Type>,
        test: Box<Type>,
    },

    /// TS2540
    EnumCannotBeLValue {
        span: Span,
    },

    /// TS2357
    ExprInvalidForUpdateArg {
        span: Span,
    },

    /// TS2356
    TypeInvalidForUpdateArg {
        span: Span,
        /// Type of the arguments.
        ty: Box<Type>,
    },

    PrivatePropertyIsDifferent {
        span: Span,
    },

    PrivateMethodIsDifferent {
        span: Span,
    },

    CannotCompareWithOp {
        span: Span,
        op: BinaryOp,
        left: Box<Type>,
        right: Box<Type>,
    },

    InvalidBinaryOp {
        span: Span,
        op: BinaryOp,
        left: Box<Type>,
        right: Box<Type>,
    },

    /// TS2339
    NoSuchEnumVariant {
        span: Span,
        name: JsWord,
    },

    ObjectIsPossiblyNull {
        span: Span,
    },

    ObjectIsPossiblyUndefined {
        span: Span,
    },

    ObjectIsPossiblyUndefinedWithType {
        span: Span,
        ty: Box<Type>,
    },

    ObjectIsPossiblyNullOrUndefined {
        span: Span,
    },

    CannotAssignAbstractConstructorToNonAbstractConstructor {
        span: Span,
    },

    InvalidUseOfConstEnum {
        span: Span,
    },

    ComputedMemberInEnumWithStrMember {
        span: Span,
    },

    CannotCreateInstanceOfAbstractClass {
        span: Span,
    },

    /// TS2344
    NotSatisfyConstraint {
        span: Span,
        left: Box<Type>,
        right: Box<Type>,
    },

    /// TS2345
    WrongArgType {
        /// Span of argument.
        span: Span,
        inner: Box<Error>,
    },

    ImportFailed {
        span: Span,
        orig: Id,
        id: Id,
    },

    /// TS7036
    NonStringDynamicImport {
        span: Span,
    },

    ExportFailed {
        span: Span,
        orig: Id,
        id: Id,
    },
    ExportAllFailed {
        span: Span,
    },
    NoSuchPropertyInThis {
        span: Span,
    },
    NoSuchPropertyInClass {
        span: Span,
        class_name: Option<Id>,
        prop: Key,
    },

    TypeParameterCountMismatch {
        span: Span,
        min: usize,
        max: usize,
        actual: usize,
    },

    ParameterCountMismatch {
        span: Span,
        min: usize,
        max: usize,
        actual: usize,
    },

    NoSuchPropertyInModule {
        span: Span,
        name: Box<Key>,
    },

    /// TS2355
    ReturnRequired {
        /// Span of the return type.
        span: Span,
    },

    ConstructorRequired {
        span: Span,
        lhs: Span,
        rhs: Span,
    },

    /// TS2539
    CannotAssignToNonVariable {
        span: Span,
        ty: Box<Type>,
    },

    /// TS2708
    CannotAssignToModule {
        span: Span,
    },

    /// TS2629
    CannotAssignToClass {
        span: Span,
    },

    /// TS2628
    CannotAssignToEnum {
        span: Span,
    },

    /// TS2630
    CannotAssignToFunction {
        span: Span,
    },

    /// TS2322
    AssignedWrapperToPrimitive {
        span: Span,
    },

    /// TS2322
    AccessibilityDiffers {
        span: Span,
    },

    /// TS2474
    InvalidInitInConstEnum {
        span: Span,
    },

    /// TS2352
    InvalidTupleCast {
        span: Span,
        left: Span,
        right: Span,
    },

    /// TS2367
    NoOverlap {
        span: Span,
        value: bool,
        left: Box<Type>,
        right: Box<Type>,
    },

    CannotAssignToReadonlyProperty {
        span: Span,
    },

    ReadOnly {
        span: Span,
    },

    ImplicitAny {
        span: Span,
    },

    /// TS7052
    ImplicitAnyBecauseNoIndexSignatureExists {
        span: Span,
    },

    /// TS7053
    ImplicitAnyBecauseIndexTypeIsWrong {
        span: Span,
    },

    /// TS7022
    ImplicitAnyBecauseOfSelfRef {
        span: Span,
    },

    TupleAssignError {
        span: Span,
        errors: Vec<Error>,
    },

    Errors {
        span: Span,
        errors: Vec<Error>,
    },

    RedeclaredVarWithDifferentType {
        span: Span,
    },

    /// TS2304
    NoSuchType {
        span: Span,
        name: Id,
    },

    /// TS2749
    NoSuchTypeButVarExists {
        span: Span,
        name: Id,
    },

    /// TS2496
    InvalidUseOfArgumentsInEs3OrEs5 {
        span: Span,
    },

    /// TS2522
    ArgumentsCannotBeUsedInAsyncFnInEs3OrEs5 {
        span: Span,
    },

    NoSuchVar {
        span: Span,
        name: Id,
    },

    /// TS2689
    CannotExtendTypeOnlyItem {
        span: Span,
        name: Id,
    },

    /// TS2693
    TypeUsedAsVar {
        span: Span,
        name: Id,
    },

    /// TS2300
    DuplicateName {
        name: Id,
        span: Span,
    },

    /// TS2300
    DuplicateNameWithoutName {
        span: Span,
    },

    /// TS2695
    UselessSeqExpr {
        span: Span,
    },

    ClassPropertyInitRequired {
        span: Span,
    },

    ReferencedInInit {
        span: Span,
    },

    NotGeneric {
        span: Span,
    },

    Unknown {
        span: Span,
    },

    NoSuchPropertyWhileDeclWithBindingPat {
        span: Span,
    },

    NoSuchProperty {
        span: Span,
        obj: Option<Box<Type>>,
        prop: Option<Box<Key>>,
    },

    NoInitAndNoDefault {
        span: Span,
    },

    TooManyTupleElements {
        span: Span,
    },

    NotTuple {
        span: Span,
    },

    /// TS2539
    NotVariable {
        // Span of rhs
        span: Span,
        left: Span,
        ty: Option<Box<Type>>,
    },

    /// TS2304
    TypeNotFound {
        name: Box<Name>,
        ctxt: ModuleId,
        type_args: Option<Box<TypeParamInstantiation>>,
        span: Span,
    },

    /// TS2378
    TS2378 {
        span: Span,
    },

    /// TS2476
    ConstEnumNonIndexAccess {
        span: Span,
    },

    /// TS2493
    TupleIndexError {
        span: Span,
        len: u64,
        index: i64,
    },

    /// TS2514
    NegativeTupleIndex {
        span: Span,
        len: u64,
        index: i64,
    },

    // TS2540
    InvalidLValue {
        span: Span,
    },

    Unimplemented {
        span: Span,
        msg: String,
    },

    ResolvedFailed {
        span: Span,
        base: Box<PathBuf>,
        src: JsWord,
    },

    MissingFields {
        span: Span,
        fields: Vec<TypeElement>,
    },

    /// TS2322
    AssignFailed {
        span: Span,
        #[derivative(Debug = "ignore")]
        left: Box<Type>,
        #[derivative(Debug = "ignore")]
        right_ident: Option<Span>,
        #[derivative(Debug = "ignore")]
        right: Box<Type>,
        cause: Vec<Error>,
    },

    /// TS2322
    AssignFailedDueToAccessibility {
        span: Span,
    },

    /// TS2322
    AssignFailedDueToOptionalityDifference {
        span: Span,
    },

    ObjectAssignFailed {
        span: Span,
        errors: Vec<Error>,
    },

    SimpleAssignFailed {
        span: Span,
        cause: Option<Box<Error>>,
    },

    SimpleAssignFailedWithCause {
        span: Span,
        cause: Vec<Error>,
    },

    InvalidAssignmentOfArray {
        span: Span,
    },

    /// a or b or c
    UnionError {
        span: Span,
        errors: Vec<Error>,
    },

    IntersectionError {
        span: Span,
        error: Box<Error>,
    },

    CannotAssignToThis {
        span: Span,
    },

    MayBeUndefined {
        /// Span of the variable
        span: Span,
    },

    UndefinedSymbol {
        sym: Id,
        span: Span,
    },

    ModuleLoadFailed {
        /// Span of the import statement.
        span: Span,
        errors: Errors,
    },

    NoSuchExport {
        span: Span,
        items: Vec<Id>,
    },

    /// TS2351
    NoNewSignature {
        span: Span,
        callee: Box<Type>,
    },

    /// TS2348
    NoConstructablePropertyWithName {
        span: Span,
        obj: Box<Type>,
        key: Box<Key>,
    },

    /// TS2349
    NoCallSignature {
        span: Span,
        callee: Box<Type>,
    },

    WrongTypeParams {
        /// Span of caller.
        span: Span,
        /// Span of callee.
        callee: Span,
        expected: RangeInclusive<usize>,
        actual: usize,
    },

    WrongParams {
        /// Span of caller.
        span: Span,
        /// Span of callee.
        callee: Span,
        expected: RangeInclusive<usize>,
        actual: usize,
    },

    InvalidEnumInit {
        span: Span,
    },

    TS1016 {
        span: Span,
    },

    TS1063 {
        span: Span,
    },

    TS1094 {
        span: Span,
    },

    TS1095 {
        span: Span,
    },

    TS1168 {
        /// Span of offending computed property.
        span: Span,
    },

    /// A computed property name in an interface must refer to an expression
    /// whose type is a literal type or a 'unique symbol' type.
    TS1169 {
        /// Span of offending computed property.
        span: Span,
    },

    TS1183 {
        span: Span,
    },

    TS1318 {
        span: Span,
    },

    TS1319 {
        span: Span,
    },

    /// TS2309
    ExportEqualsMixedWithOtherExports {
        span: Span,
    },

    AnyTypeUsedAsCalleeWithTypeArgs {
        span: Span,
    },

    /// TS2360
    InvalidLhsOfInOperator {
        span: Span,
    },

    InvalidRhsForInOperator {
        span: Span,
        ty: Box<Type>,
    },

    /// TS2362
    WrongTypeForLhsOfNumericOperation {
        span: Span,
    },

    /// TS2363
    WrongTypeForRhsOfNumericOperation {
        span: Span,
        ty: Box<Type>,
    },

    TS2365 {
        span: Span,
    },

    TS2370 {
        span: Span,
    },

    /// TS2394
    WrongOverloadSignature {
        span: Span,
    },

    TS1166 {
        span: Span,
    },

    TS1345 {
        span: Span,
    },

    TS2353 {
        span: Span,
    },

    /// TS2390
    ConstructorImplMissingOrNotFollowedByDecl {
        span: Span,
    },

    /// TS2391
    FnImplMissingOrNotFollowedByDecl {
        span: Span,
    },

    /// TS2464
    InvalidTypeForComputedProperty {
        span: Span,
        ty: Box<Type>,
    },

    /// TS2369
    ParamPropIsNotAllowedInAmbientConstructor {
        span: Span,
    },

    TS2389 {
        span: Span,
    },

    TS2447 {
        span: Span,
    },

    /// TS2515
    ClassDoesNotImplementMember {
        span: Span,
        key: Box<Key>,
    },

    TS2531 {
        span: Span,
    },

    TS2567 {
        span: Span,
    },

    /// Type used as a variable, but changing target library can fix the issue.
    TS2585 {
        span: Span,
    },

    TS2704 {
        span: Span,
    },

    /// `TS2358`
    InvalidLhsInInstanceOf {
        span: Span,
        /// Type of the lhs
        ty: Box<Type>,
    },

    /// `TS2359`
    InvalidRhsInInstanceOf {
        span: Span,
        /// Type of the rhs
        ty: Box<Type>,
    },

    /// `TS2469`
    NumericOpToSymbol {
        /// Span of the argument.
        span: Span,
    },

    /// TS2356
    InvalidNumericOperand {
        span: Span,
    },

    /// `TS2469`
    UpdateOpToSymbol {
        /// Span of the argument.
        span: Span,
        op: UpdateOp,
    },

    UnknownPropertyInObjectLiteralAssignment {
        span: Span,
    },

    NonOverlappingTypeCast {
        span: Span,
        from: Box<Type>,
        to: Box<Type>,
    },

    InvalidOperatorForLhs {
        span: Span,
        op: AssignOp,
    },

    InvalidOpAssign {
        span: Span,
        op: AssignOp,
        lhs: Box<Type>,
        rhs: Box<Type>,
    },

    AssignOpCannotBeApplied {
        span: Span,
        op: AssignOp,
    },

    ExpectedNArgsButGotM {
        span: Span,
        min: usize,
        max: Option<usize>,
    },

    /// TS2555
    ExpectedAtLeastNArgsButGotM {
        span: Span,
        min: usize,
        // param name needed for error message
        param_name: JsWord,
    },

    ExpectedAtLeastNArgsButGotMOrMore {
        span: Span,
        min: usize,
    },

    ExpectedNArgsButGotMOrMore {
        span: Span,
    },

    /// TS2556
    SpreadMustBeTupleOrPassedToRest {
        span: Span,
    },

    /// TS2703
    InvalidDeleteOperand {
        span: Span,
    },

    /// TS2489
    NoMethodNamedNext {
        span: Span,
    },

    /// TS2489
    NoCallablePropertyWithName {
        span: Span,
        obj: Box<Type>,
        key: Box<Key>,
    },

    /// TS2461
    MustBeArray {
        span: Span,
    },

    /// TS2548
    MustHaveSymbolIteratorThatReturnsIteratorOrMustBeArray {
        span: Span,
    },

    /// TS2488
    MustHaveSymbolIteratorThatReturnsIterator {
        span: Span,
    },

    /// TS2407
    RightHandSideMustBeObject {
        span: Span,
        ty: Box<Type>,
    },

    MustHaveSymbolAsyncIteratorThatReturnsIterator {
        span: Span,
    },

    NoSuchConstructor {
        span: Span,
        key: Box<Key>,
    },

    /// TS2512
    AbstractAndConcreteIsMixed {
        span: Span,
    },

    /// TS2340
    SuperCanOnlyAccessPublicAndProtectedMethod {
        span: Span,
    },

    /// TS2387
    ShouldBeStaticMethod {
        span: Span,
    },

    /// TS2388
    ShouldBeInstanceMethod {
        span: Span,
    },

    /// TS2462
    RestPropertyNotLast {
        span: Span,
    },

    /// TS2673
    ClassConstructorPrivate {
        span: Span,
    },

    /// TS2674
    ClassConstructorProtected {
        span: Span,
    },

    /// TS2675
    InvalidExtendDueToConstructorPrivate {
        span: Span,
    },

    /// TS2804
    DuplicatePrivateStaticInstance {
        span: Span,
    },

    /// TS2668
    ExportAmbientModule {
        span: Span,
    },

    /// TS18046
    IsTypeUnknown {
        span: Span,
    },

    /// TS2700
    RestTypeNotFromObject {
        span: Span,
    },

    /// TS2729
    UsePropBeforeInit {
        span: Span,
        obj: Option<Box<Type>>,
        prop: Option<Box<Key>>,
    },

    /// TS4113
    NotDeclaredInSuperClass {
        span: Span,
    },

    /// TS2428
    InterfaceNonIdenticalTypeParams {
        span: Span,
    },

    /// TS2784
    ThisNotAllowedInAccessor {
        span: Span,
    },

    /// TS1014
    RestParamMustBeLast {
        span: Span,
    },
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(ErrorKind, [u8; 72]);

impl Error {
    pub fn convert<F>(mut self, op: F) -> Self
    where
        F: FnOnce(ErrorKind) -> ErrorKind,
    {
        self.inner = box op(*self.inner);
        self
    }

    /// Convert all errors if `self` is [Error::Errors] and convert itself
    /// otherwise.
    pub fn convert_all<F>(self, mut op: F) -> Self
    where
        F: FnMut(Self) -> Self,
    {
        self.convert_all_inner(&mut op)
    }

    fn convert_all_inner<F>(self, op: &mut F) -> Self
    where
        F: FnMut(Self) -> Self,
    {
        match *self.inner {
            ErrorKind::Errors { span, errors } => {
                let mut new = Vec::with_capacity(errors.capacity());
                for err in errors {
                    new.push(err.convert_all_inner(op));
                }

                ErrorKind::Errors { span, errors: new }.into()
            }

            _ => op(self),
        }
    }
}

impl ErrorKind {
    pub fn normalize_error_code(code: usize) -> usize {
        match code {
            // TS2304: Type not found.
            // TS2318: Type not found and name is global.
            // TS2552: Type not found with recommendation.
            // TS2580: Type not found with recommendation for package to install.
            // TS2581: Type not found with recommendation for jQuery.
            // TS2582: Type not found with recommendation for jest or mocha.
            // TS2583: Type not found with recommendation to change target library.
            // TS2584: Type not found with recommendation to change target library to include `dom`.
            2318 | 2552 | 2580 | 2581 | 2582 | 2583 | 2584 => 2304,

            // TS2348: Not callable, but with a suggestion to use new
            // TS2349: Not callable
            2348 | 2349 => 2349,

            // TS2350: new cannot be used for non-void function
            // TS2350: new cannot be used because it's not constructor
            2350 | 2351 => 2350,

            // TS2339: Property not found.
            // TS2550: Property not found with a suggestion to change `lib`.
            // TS2551: Property not found with a suggestion.
            2550 | 2551 => 2339,

            // TS2304: Variable not found
            // TS2585: Variable not found, but with a suggestion to change 'lib',
            // TS2693: Variable not found, but a type with same name exists.
            2304 | 2585 | 2693 => 2304,

            // TS2307: Module not found.
            // TS2792: Module not found with recommendation to change module resolution.
            2792 => 2307,

            // TS2372: Referenced while initialization.
            // TS2448: Referenced while initialization and the variable is declared with let or
            // const.
            2448 => 2372,

            // ===== ===== ===== For convenience ===== ===== =====

            // TS2461: Not an array type.
            // TS2488: Need Symbol.iterator
            // TS2548: Not an array or no Symbol.iterator
            // TS2549: Not an array, string or no Symbol.iterator
            // TS2569: Not an array, string or no Symbol.iterator but downlevel iteration will work.
            2461 | 2488 | 2548 | 2549 | 2569 => 2461,

            // TS7005; No implicit any for variables.
            // TS7006; No implicit any for parameters.
            // TS7008; No implicit any for members.
            // TS7031; No implicit any for binding patterns.
            // TS7032; No implicit any for set accessor.
            // TS7033; No implicit any for get accessor.
            // TS7034; No implicit any for "in some locations where its type cannot be determined."
            7005 | 7006 | 7008 | 7031 | 7032 | 7033 | 7034 => 7005,

            // TS2532: Object is possibly 'undefined'.
            // TS18048: ${obj} is possibly 'undefined'.
            2532 | 18048 => 2532,

            // TS2531: Object is possibly 'null'.
            // TS18047: ${obj} is possibly 'null'.
            2531 | 18047 => 2531,

            // TS2571: Object is of type 'unknown'
            2571 => 2531,

            // TS2533: Object is possibly 'null' or 'undefined'.
            // TS18049: '{0}' is possibly 'null' or 'undefined'.
            2533 | 18049 => 2533,

            // TS2739: Missing properties with a type name
            // TS2740: Missing properties with type names
            // TS2741: Missing properties with comparison-like error message
            2739 | 2740 | 2741 => 2322,

            // TS4113: Cannot have override
            // TS4117: Cannot have override with spelling suggestion
            4113 | 4117 => 4112,

            // TS2515: Missing an abstract member
            // TS18052: Missing all abstract members
            2515 | 18052 => 2515,

            // TS5101: Deprecated
            // TS5107: Deprecated with two args
            5101 | 5107 => 5101,

            _ => code,
        }
    }

    #[track_caller]
    pub fn context(self, context: impl Display) -> Error {
        if !cfg!(debug_assertions) {
            return self.into();
        }

        match self {
            ErrorKind::Errors { .. } => {}
            _ => {
                if self.span().is_dummy() {
                    unreachable!("Error with dummy span found(context: {}): {:#?}", context, self)
                }
            }
        }
        let loc = Location::caller();

        let err: Error = self.into();
        err.context_impl(loc, context)
    }

    /// Split error into causes.
    pub fn into_causes(self) -> Vec<Error> {
        match self {
            Self::AssignFailed { cause, .. } => cause,
            Self::ObjectAssignFailed { errors, .. } => errors,
            _ => {
                vec![Error {
                    #[cfg(debug_assertions)]
                    contexts: Default::default(),
                    inner: box self,
                }]
            }
        }
    }

    /// TypeScript error code.
    pub fn code(&self) -> usize {
        match self {
            ErrorKind::TS1016 { .. } => 1016,
            ErrorKind::TS1063 { .. } => 1063,
            ErrorKind::TS1094 { .. } => 1094,
            ErrorKind::TS1095 { .. } => 1095,
            ErrorKind::TS1168 { .. } => 1168,
            ErrorKind::TS1169 { .. } => 1169,
            ErrorKind::TS1183 { .. } => 1183,
            ErrorKind::TS1318 { .. } => 1318,
            ErrorKind::TS1319 { .. } => 1319,
            ErrorKind::ExportEqualsMixedWithOtherExports { .. } => 2309,
            ErrorKind::AnyTypeUsedAsCalleeWithTypeArgs { .. } => 2347,
            ErrorKind::InvalidLhsOfInOperator { .. } => 2360,
            ErrorKind::InvalidRhsForInOperator { .. } => 2638,
            ErrorKind::WrongTypeForLhsOfNumericOperation { .. } => 2362,
            ErrorKind::WrongTypeForRhsOfNumericOperation { .. } => 2363,
            ErrorKind::TS2365 { .. } => 2365,
            ErrorKind::TS2370 { .. } => 2370,
            ErrorKind::WrongOverloadSignature { .. } => 2394,
            ErrorKind::TS1166 { .. } => 1166,
            ErrorKind::TS1345 { .. } => 1345,
            ErrorKind::TS2353 { .. } => 2353,
            ErrorKind::ConstructorImplMissingOrNotFollowedByDecl { .. } => 2390,
            ErrorKind::FnImplMissingOrNotFollowedByDecl { .. } => 2391,
            ErrorKind::InvalidTypeForComputedProperty { .. } => 2464,

            ErrorKind::ParamPropIsNotAllowedInAmbientConstructor { .. } => 2369,
            ErrorKind::TS2389 { .. } => 2389,
            ErrorKind::TS2447 { .. } => 2447,
            ErrorKind::ClassDoesNotImplementMember { .. } => 2515,
            ErrorKind::TS2531 { .. } => 2531,
            ErrorKind::TS2567 { .. } => 2567,
            ErrorKind::TS2585 { .. } => 2585,
            ErrorKind::TS2704 { .. } => 2704,

            ErrorKind::AssignFailed { .. }
            | ErrorKind::AssignFailedDueToAccessibility { .. }
            | ErrorKind::ObjectAssignFailed { .. }
            | ErrorKind::SimpleAssignFailed { .. }
            | ErrorKind::SimpleAssignFailedWithCause { .. }
            | ErrorKind::InvalidAssignmentOfArray { .. }
            | ErrorKind::UnknownPropertyInObjectLiteralAssignment { .. }
            | ErrorKind::InvalidOpAssign { .. }
            | ErrorKind::TupleAssignError { .. } => 2322,

            ErrorKind::NonOverlappingTypeCast { .. } => 2352,

            ErrorKind::SuperInClassWithoutSuper { .. } => 2335,

            ErrorKind::SuperCanBeOnlyReferencedInDerivedClass { .. } => 2660,

            ErrorKind::NoSuchProperty { .. }
            | ErrorKind::NoSuchPropertyInThis { .. }
            | ErrorKind::NoSuchPropertyInClass { .. }
            | ErrorKind::NoSuchPropertyInModule { .. } => 2339,

            ErrorKind::AssignOpCannotBeApplied { .. } => 2365,
            ErrorKind::TypeUsedAsVar { .. } => 2693,
            ErrorKind::CannotExtendTypeOnlyItem { .. } => 2689,
            ErrorKind::TypeNotFound { .. } => 2304,

            ErrorKind::NotVariable { ty, .. } => match ty.as_deref().map(Type::normalize) {
                Some(Type::Enum(..)) => 2628,
                Some(Type::ClassDef(..)) => 2629,
                Some(Type::Function(..)) => 2630,
                Some(Type::Module(..)) => 2631,
                Some(Type::Import(..)) => 2632,
                _ => 2539,
            },

            ErrorKind::NoInitAndNoDefault { .. } => 2525,

            ErrorKind::ExpectedNArgsButGotM { .. } => 2554,
            ErrorKind::ExpectedAtLeastNArgsButGotM { .. } => 2555,
            ErrorKind::ExpectedNArgsButGotMOrMore { .. } => 2556,
            ErrorKind::ExpectedAtLeastNArgsButGotMOrMore { .. } => 2557,
            ErrorKind::SpreadMustBeTupleOrPassedToRest { .. } => 2556,

            ErrorKind::TypeParameterCountMismatch { .. } => 2558,

            ErrorKind::ReferencedInInit { .. } => 2372,

            ErrorKind::InvalidDeleteOperand { .. } => 2703,

            ErrorKind::DuplicateName { .. } | ErrorKind::DuplicateNameWithoutName { .. } => 2300,

            ErrorKind::NoSuchVar { .. } => 2304,
            ErrorKind::NoSuchType { .. } => 2304,
            ErrorKind::NoSuchTypeButVarExists { .. } => 2749,
            ErrorKind::NoSuchVarButThisHasSuchProperty { .. } => 2663,

            ErrorKind::CannotAssignAbstractConstructorToNonAbstractConstructor { .. } => 2322,
            ErrorKind::CannotCreateInstanceOfAbstractClass { .. } => 2511,
            ErrorKind::NotSatisfyConstraint { .. } => 2344,
            ErrorKind::WrongArgType { .. } => 2345,

            ErrorKind::ComputedMemberInEnumWithStrMember { .. } => 2553,

            ErrorKind::TupleIndexError { .. } => 2493,
            ErrorKind::NegativeTupleIndex { .. } => 2514,
            ErrorKind::InvalidLValue { .. } => 2540,

            ErrorKind::TS2378 { .. } => 2378,

            ErrorKind::ConstEnumNonIndexAccess { .. } => 2476,

            ErrorKind::InvalidUseOfConstEnum { .. } => 2475,

            ErrorKind::ObjectIsPossiblyNull { .. } => 2531,
            ErrorKind::ObjectIsPossiblyUndefined { .. } | ErrorKind::ObjectIsPossiblyUndefinedWithType { .. } => 2532,
            ErrorKind::ObjectIsPossiblyNullOrUndefined { .. } => 2533,

            ErrorKind::InvalidBinaryOp { .. } => 2365,

            ErrorKind::CannotCompareWithOp { .. } => 2365,

            ErrorKind::TypeInvalidForUpdateArg { .. } => 2356,
            ErrorKind::ExprInvalidForUpdateArg { .. } => 2357,

            ErrorKind::PrivatePropertyIsDifferent { .. } => 2415,

            ErrorKind::CannotAssignToNonVariable { .. } => 2539,
            ErrorKind::CannotAssignToModule { .. } => 2708,
            ErrorKind::CannotAssignToClass { .. } => 2629,
            ErrorKind::CannotAssignToEnum { .. } => 2628,
            ErrorKind::CannotAssignToFunction { .. } => 2630,

            ErrorKind::AssignedWrapperToPrimitive { .. } => 2322,

            ErrorKind::AccessibilityDiffers { .. } => 2322,

            ErrorKind::InvalidInitInConstEnum { .. } => 2474,

            ErrorKind::InvalidTupleCast { .. } => 2352,

            ErrorKind::NoOverlap { .. } => 2367,

            ErrorKind::InvalidLhsInInstanceOf { .. } => 2358,

            ErrorKind::InvalidRhsInInstanceOf { .. } => 2359,

            ErrorKind::NumericOpToSymbol { .. } => 2469,

            ErrorKind::InvalidNumericOperand { .. } => 2356,

            ErrorKind::UpdateOpToSymbol { .. } => 2469,

            ErrorKind::UselessSeqExpr { .. } => 2695,

            ErrorKind::EnumCannotBeLValue { .. } => 2540,

            ErrorKind::NoSuchEnumVariant { .. } => 2339,

            ErrorKind::TupleTooShort { .. } => 2339,

            ErrorKind::SwitchCaseTestNotCompatible { .. } => 2678,

            ErrorKind::NullishCoalescingMixedWithLogicalWithoutParen { .. } => 5076,

            ErrorKind::OptionalBindingPatternInImplSignature { .. } => 2463,

            ErrorKind::GeneratorCannotHaveVoidAsReturnType { .. } => 2505,

            ErrorKind::MissingFields { .. } => 2741,

            ErrorKind::MustHaveSymbolIteratorThatReturnsIterator { .. } => 2488,

            ErrorKind::RightHandSideMustBeObject { .. } => 2407,

            ErrorKind::MustHaveSymbolAsyncIteratorThatReturnsIterator { .. } => 2504,

            ErrorKind::MustHaveSymbolIteratorThatReturnsIteratorOrMustBeArray { .. } => 2548,

            ErrorKind::NoSuchPropertyWhileDeclWithBindingPat { .. } => 2525,

            ErrorKind::NoNewSignature { .. } => 2351,

            ErrorKind::Unknown { .. } => 2571,

            ErrorKind::ReturnRequired { .. } => 2355,

            ErrorKind::ThisRefToModuleOrNamespace { .. } => 2331,

            ErrorKind::CannotReferenceThisInComputedPropName { .. } => 2465,
            ErrorKind::CannotReferenceSuperInComputedPropName { .. } => 2466,
            ErrorKind::DeclaringTypeParamReferencedByComputedPropName { .. } => 2467,

            ErrorKind::StaticMemberCannotUseTypeParamOfClass { .. } => 2302,

            ErrorKind::InvalidImplOfInterface { .. } => 2420,

            ErrorKind::ClassIncorrectlyImplementsInterface { .. } => 2420,

            ErrorKind::MixedDefaultExports { .. } => 2652,

            ErrorKind::ExportMixedWithLocal { .. } => 2395,

            ErrorKind::NotConstructorType { .. } => 2507,

            ErrorKind::SelfReferentialSuperClass { .. } => 2506,

            ErrorKind::StaticPropertyCannotBeNamedPrototype { .. } => 2699,

            ErrorKind::CannotExportNonLocalVar { .. } => 2661,

            ErrorKind::DuplicateProperty { .. } => 2300,

            ErrorKind::CannotCallWithNewNonVoidFunction { .. } => 2350,

            ErrorKind::InvalidInterfaceName { .. } => 2427,

            ErrorKind::InvalidUseOfArgumentsInEs3OrEs5 { .. } => 2496,

            ErrorKind::ArgumentsCannotBeUsedInAsyncFnInEs3OrEs5 { .. } => 2522,

            ErrorKind::NoMatchingOverload { .. } => 2769,

            ErrorKind::NoSuchVarForShorthand { .. } => 18004,

            ErrorKind::NoCallSignature { .. } => 2349,

            ErrorKind::InvalidClassName { .. } => 2414,

            ErrorKind::InitializerDisallowedInAmbientContext { .. } => 2371,

            ErrorKind::IncompatibleFnOverload { .. } => 2394,

            ErrorKind::ImplicitReturnType { .. } => 7010,

            ErrorKind::InvalidLhsOfAssign { .. } => 2364,

            ErrorKind::EnumMemberIdCannotBeNumber { .. } => 2452,

            ErrorKind::NamespaceNotFound { .. } => 2503,

            ErrorKind::WithStmtNotSupported { .. } => 2410,

            ErrorKind::InvalidSuperClass { .. } => 2507,

            ErrorKind::ThisInConstructorParam { .. } => 2333,

            ErrorKind::ThisInStaticPropertyInitializer { .. } => 2334,

            ErrorKind::ImplicitAny { .. } => 7005,

            ErrorKind::ImplicitAnyBecauseNoIndexSignatureExists { .. } => 7052,

            ErrorKind::ImplicitAnyBecauseIndexTypeIsWrong { .. } => 7053,

            ErrorKind::ImplicitAnyBecauseOfSelfRef { .. } => 7022,

            ErrorKind::ConstructorIsKeyword { .. } => 18012,

            ErrorKind::PrivateIdUsedAsMethodName { .. } => 18022,

            ErrorKind::UndefinedOrNullIsNotValidOperand { .. } => 18050,

            ErrorKind::CannotDeletePrivateProperty { .. } => 18011,

            ErrorKind::CannotAccessPrivatePropertyFromOutside { .. } => 18013,

            ErrorKind::OptionalChainCannotContainPrivateIdentifier { .. } => 18030,

            ErrorKind::TypeAnnOnLhsOfForInLoops { .. } => 2404,
            ErrorKind::TypeAnnOnLhsOfForOfLoops { .. } => 2483,

            ErrorKind::DestructuringBindingNotAllowedInLhsOfForIn { .. } => 2491,

            ErrorKind::WrongTypeForLhsOfForInLoop { .. } => 2405,

            ErrorKind::InvalidExprOfLhsOfForIn { .. } => 2406,
            ErrorKind::InvalidExprOfLhsOfForOf { .. } => 2487,

            ErrorKind::LetOrConstIsNotValidIdInLetOrConstVarDecls { .. } => 2480,
            ErrorKind::ForOfStringUsedInEs3 { .. } => 2494,

            ErrorKind::NotArrayType { .. } => 2461,
            ErrorKind::NotArrayTypeNorStringType { .. } => 2495,
            ErrorKind::NotArrayTypeNorStringTypeButDownlevelIterationWouldWork { .. } => 2569,

            ErrorKind::NoConstructablePropertyWithName { .. } => 2348,

            ErrorKind::NoCallablePropertyWithName { .. } => 2349,

            ErrorKind::MustBeArray { .. } => 2461,

            ErrorKind::NoMethodNamedNext { .. } => 2489,

            ErrorKind::NotGeneric { .. } => 2315,

            ErrorKind::CannotAssignToReadonlyProperty { .. } => 2540,

            ErrorKind::ReadOnly { .. } => 2540,

            ErrorKind::ClassNameCannotBeObjectWhenTargetingEs5WithModule { .. } => 2725,

            ErrorKind::DuplicateVar { .. } => 2451,

            ErrorKind::TooManyAsterisk { .. } => 5061,

            ErrorKind::ModuleNotFound { .. } => 2307,

            ErrorKind::DuplicateConstructor { .. } => 2392,

            ErrorKind::DuplicateFnImpl { .. } => 2393,

            ErrorKind::DuplicateDefaultExport { .. } => 2528,
            ErrorKind::DuplicateExport { .. } => 2323,

            ErrorKind::BlockScopedVarUsedBeforeInit { .. } => 2448,

            ErrorKind::SuperCannotUseTypeArgs { .. } => 2754,

            ErrorKind::DeleteOperandMustBeOptional { .. } => 2790,

            ErrorKind::BindingPatNotAllowedInRestPatArg { .. } => 2501,

            ErrorKind::RestArgMustBeVarOrMemberAccess { .. } => 2701,

            ErrorKind::CannotAssignToNamespace { .. } => 2631,

            ErrorKind::ReturnPropertyOfIteratorMustBeMethod { .. } => 2767,

            ErrorKind::NextOfIteratorShouldReturnTypeWithPropertyValue { .. } => 2490,

            ErrorKind::InvalidUsageOfNewTarget { .. } => 17013,

            ErrorKind::AssignFailedBecauseTupleLengthDiffers { .. } => 2322,

            ErrorKind::ClassMemberNotCompatibleWithStringIndexSignature { .. } => 2411,

            ErrorKind::ClassMemberNotCompatibleWithNumericIndexSignature { .. } => 2411,

            ErrorKind::AbstractAndConcreteIsMixed { .. } => 2512,

            ErrorKind::AbstractClassMethodShouldBeSequential { .. } => 2516,

            ErrorKind::OperatorCannotBeAppliedToTypes { .. } => 2365,

            ErrorKind::CannotAccessAbstractMember { .. } => 2513,

            ErrorKind::SuperNotCalled { .. } => 2377,

            ErrorKind::SuperInNestedFunction { .. } => 2337,

            ErrorKind::InvalidOperandOfIncDecOptionalProp { .. } => 2777,

            ErrorKind::InvalidRestPatternInOptionalChain { .. } => 2778,

            ErrorKind::InvalidLhsOfAssignOptionalProp { .. } => 2779,

            ErrorKind::InvalidRestPatternInForIn { .. } => 2780,

            ErrorKind::InvalidRestPatternInForOf { .. } => 2781,

            ErrorKind::ThisUsedBeforeCallingSuper { .. } => 17009,

            ErrorKind::SuperUsedBeforeCallingSuper { .. } => 17011,

            ErrorKind::OptionInvalidForEs3 { .. } => 5048,

            ErrorKind::ShouldBeStaticMethod { .. } => 2387,

            ErrorKind::ShouldBeInstanceMethod { .. } => 2388,

            ErrorKind::RestPropertyNotLast { .. } => 2462,

            ErrorKind::DefinedWithAccessorInSuper { .. } => 2610,

            ErrorKind::ClassPropNotInitialized { .. } => 2564,

            ErrorKind::VarMayNotBeInitialized { .. } => 2454,

            ErrorKind::UnreachableCode { .. } => 7027,

            ErrorKind::ConstEnumMemberHasInfinityAsInit { .. } => 2477,

            ErrorKind::ConstEnumMemberHasNaNAsInit { .. } => 2478,

            ErrorKind::OnlyOneEnumCanOmitInit { .. } => 2432,

            ErrorKind::CannotUseTypeAsIndexIndex { .. } => 2538,
            ErrorKind::CannotFunctionReturningNever { .. } => 2534,
            ErrorKind::InterfaceNotCompatible { .. } => 2320,

            ErrorKind::UpdateArgMustBeVariableOrPropertyAccess { .. } => 2357,

            ErrorKind::OptionalAndNonOptionalMethodPropertyMixed { .. } => 2386,

            ErrorKind::TypeParamsProvidedButCalleeIsNotGeneric { .. } => 2347,

            ErrorKind::IntrinsicIsBuiltinOnly { .. } => 2795,

            ErrorKind::VarDeclNotCompatible { .. } => 2403,

            ErrorKind::InvalidInterfaceInheritance { .. } => 2430,

            ErrorKind::TargetLacksConstructSignature { .. } => 7009,

            ErrorKind::NotExtendableType { .. } => 2312,

            ErrorKind::NonObjectInSpread { .. } => 2698,

            ErrorKind::TypeCannotBeUsedForIndex { .. } => 2538,

            ErrorKind::ImplicitAnyBecauseThereIsNoJsxInterface { .. } => 7026,

            ErrorKind::NoCommonProperty { .. } => 2559,

            ErrorKind::ClassConstructorPrivate { .. } => 2673,

            ErrorKind::ClassConstructorProtected { .. } => 2674,

            ErrorKind::InvalidExtendDueToConstructorPrivate { .. } => 2675,

            ErrorKind::SuperCanOnlyAccessPublicAndProtectedMethod { .. } => 2340,

            ErrorKind::DuplicatePrivateStaticInstance { .. } => 2804,

            ErrorKind::ExportAmbientModule { .. } => 2668,

            ErrorKind::IsTypeUnknown { .. } => 18046,

            ErrorKind::RestTypeNotFromObject { .. } => 2700,

            ErrorKind::UsePropBeforeInit { .. } => 2729,

            ErrorKind::NonStringDynamicImport { .. } => 7036,

            ErrorKind::NotDeclaredInSuperClass { .. } => 4113,

            ErrorKind::InterfaceNonIdenticalTypeParams { .. } => 2428,

            ErrorKind::ThisNotAllowedInAccessor { .. } => 2784,

            ErrorKind::RestParamMustBeLast { .. } => 1014,

            _ => 0,
        }
    }

    pub fn is_property_not_found(&self) -> bool {
        matches!(
            self,
            ErrorKind::NoSuchProperty { .. }
                | ErrorKind::NoSuchPropertyInClass { .. }
                | ErrorKind::NoSuchPropertyInModule { .. }
                | ErrorKind::NoSuchPropertyInThis { .. }
        )
    }

    pub fn is_var_not_found(&self) -> bool {
        matches!(
            self,
            Self::NoSuchVar { .. } | Self::NoSuchVarButThisHasSuchProperty { .. } | Self::NoSuchVarForShorthand { .. }
        )
    }

    pub fn is_readonly_error(&self) -> bool {
        self.code() == 2540
    }

    pub fn is_assign_failure(&self) -> bool {
        self.code() == 2322
    }

    pub fn is_type_not_found(&self) -> bool {
        matches!(self, Self::NoSuchType { .. } | Self::NoSuchTypeButVarExists { .. })
    }

    #[cold]
    pub fn flatten(vec: Vec<Error>) -> Vec<Error> {
        let mut buf = Vec::with_capacity(vec.len());

        for e in vec {
            match *e.inner {
                ErrorKind::Errors { errors, .. } | ErrorKind::TupleAssignError { errors, .. } => {
                    buf.extend(Self::flatten(errors).into_iter().map(|mut err| {
                        #[cfg(debug_assertions)]
                        for context in &e.contexts {
                            err.contexts.push(context.clone());
                        }

                        err
                    }))
                }
                _ => buf.push(e),
            }
        }

        buf
    }
}

impl From<Vec<Error>> for ErrorKind {
    #[inline]
    fn from(errors: Vec<Error>) -> Self {
        ErrorKind::Errors { span: DUMMY_SP, errors }
    }
}

impl From<Errors> for ErrorKind {
    #[inline]
    fn from(errors: Errors) -> Self {
        errors.0.into()
    }
}

/// A utility type to track
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Errors(Vec<Error>);

impl From<Errors> for Vec<Error> {
    #[inline]
    fn from(e: Errors) -> Self {
        e.0
    }
}

impl IntoIterator for Errors {
    type IntoIter = <Vec<Error> as IntoIterator>::IntoIter;
    type Item = Error;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl Errors {
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[inline]
    pub fn push(&mut self, err: Error) {
        self.validate(&err);

        self.0.push(err);
    }

    #[inline]
    pub fn reserve(&mut self, v: usize) {
        self.0.reserve(v)
    }

    #[inline]
    pub fn append(&mut self, other: &mut Vec<Error>) {
        for err in &*other {
            self.validate(err)
        }

        self.0.append(other)
    }

    #[inline]
    pub fn append_errors(&mut self, other: &mut Self) {
        self.append(&mut other.0)
    }
}

impl Extend<Error> for Errors {
    #[inline]
    fn extend<T: IntoIterator<Item = Error>>(&mut self, iter: T) {
        if cfg!(debug_assertions) {
            for err in iter {
                self.push(err)
            }
        } else {
            self.0.extend(iter)
        }
    }
}

impl From<StackOverflowError> for ErrorKind {
    fn from(e: StackOverflowError) -> Self {
        ErrorKind::StackOverflow { span: e.span }
    }
}

impl From<StackOverflowError> for Error {
    fn from(e: StackOverflowError) -> Self {
        ErrorKind::from(e).into()
    }
}
