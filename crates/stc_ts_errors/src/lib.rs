#![allow(incomplete_features)]
#![deny(variant_size_differences)]
#![feature(box_syntax)]
#![feature(specialization)]

use std::{
    borrow::Cow,
    fmt,
    fmt::{Debug, Display},
    ops::RangeInclusive,
    path::PathBuf,
};

use ansi_term::Color::Yellow;
use derivative::Derivative;
use fmt::Formatter;
use static_assertions::assert_eq_size;
use stc_ts_ast_rnode::RTsModuleName;
use stc_ts_types::{name::Name, Id, Key, ModuleId, Type, TypeElement, TypeParamInstantiation};
use stc_utils::stack::StackOverflowError;
use swc_atoms::JsWord;
use swc_common::{
    errors::{DiagnosticId, Handler},
    Span, Spanned, DUMMY_SP,
};
use swc_ecma_ast::{AssignOp, BinaryOp, UnaryOp, UpdateOp};

pub use self::result_ext::DebugExt;

pub mod debug;
mod result_ext;

impl Errors {
    /// This is used for debugging (by calling [pacic]).
    fn validate(&self, err: &Error) {
        if let Ok(var) = std::env::var("DBG_ERROR") {
            let s = format!("{:?}", err);
            if var != "" && s.contains(&var) {
                crate::debug::print_backtrace();
            }
        }

        match err {
            // Error::UndefinedSymbol { .. } => panic!(),
            Error::Errors { ref errors, .. } => {
                for err in errors {
                    self.validate(err)
                }
                return;
            }
            Error::DebugContext { .. } => return,
            _ => {}
        }

        let code = err.code();
        if 5000 <= code && code < 6000 {
            // This is error for invalid options.
        } else if err.span().is_dummy() {
            unreachable!("Error with a dummy span found: {:?}", err)
        }
    }
}

#[derive(Derivative, Clone, PartialEq, Spanned)]
#[derivative(Debug)]
pub enum Error {
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

    /// TS2432
    OnlyOneEnumCanOmitInit {
        span: Span,
    },

    /// TS2477
    ConstEnumMemberHasInifinityAsInit {
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
    DefinedWitHAccessorInSuper {
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
    AbstractClassMethodShouldBeSequntial {
        span: Span,
    },

    /// TS2411
    ClassMemberNotCompatibleWithStringIndexSignature {
        span: Span,
    },

    /// TS2412
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
    NextOfItertorShouldReturnTypeWithPropertyValue {
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
    NamspaceNotFound {
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
    ImcompatibleFnOverload {
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

    SuperInClassWithoutSuper {
        span: Span,
    },

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

    OptionalBindingPatternInImplSignature {
        span: Span,
    },

    NullishCoalescingMixedWithLogicalWithoutParen {
        span: Span,
    },

    /// TS2678
    SwitchCaseTestNotCompatible {
        span: Span,
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
        name: Box<RTsModuleName>,
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
        left: Span,
        right: Span,
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

    NoSuchPropertyWhileDeclWithBidningPat {
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

    // TS2493
    TupleIndexError {
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

    CannotAssingToThis {
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

    TS2360 {
        span: Span,
    },

    TS2361 {
        span: Span,
    },

    /// TS2362
    WrongTypeForLhsOfNumericOperation {
        span: Span,
    },

    /// TS2363
    WrongTypeForRhsOfNumericOperation {
        span: Span,
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
    ParamPropIsNotAllowedInAmbientConstructorx {
        span: Span,
    },

    TS2389 {
        span: Span,
    },

    TS2447 {
        span: Span,
    },

    /// TS2515
    ClassDoesNotImplementMemeber {
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
    NumericUnaryOpToSymbol {
        /// Span of the argument.
        span: Span,
        op: UnaryOp,
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

    /// TS2471
    NonSymbolComputedPropInFormOfSymbol {
        span: Span,
    },

    ExpectedNArgsButGotM {
        span: Span,
        min: usize,
        max: Option<usize>,
    },

    ExpectedAtLeastNArgsButGotM {
        span: Span,
        min: usize,
    },

    ExpectedAtLeastNArgsButGotMOrMore {
        span: Span,
        min: usize,
    },

    ExpectedNArgsButGotMOrMore {
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

    NoCallablePropertyWithName {
        span: Span,
        obj: Box<Type>,
        key: Box<Key>,
    },

    /// TS2548
    MustHaveSymbolIteratorThatReturnsIteratorOrMustBeArray {
        span: Span,
    },

    MustHaveSymbolIteratorThatReturnsIterator {
        span: Span,
    },

    MustHaveSymbolAsycIteratorThatReturnsIterator {
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

    /// TS2387
    ShouldBeStaticMethod {
        span: Span,
    },

    /// TS2388
    ShouldBeInstanceMethod {
        span: Span,
    },

    DebugContext(DebugContext),
}

assert_eq_size!(Error, [u8; 88]);

impl Error {
    pub fn convert<F>(self, op: F) -> Self
    where
        F: FnOnce(Self) -> Self,
    {
        match self {
            Error::DebugContext(c) => {
                let c = c.convert(op);
                Error::DebugContext(c)
            }
            _ => op(self),
        }
    }

    /// Convert all errors if `self` is [Error::Errors] and convert itself
    /// otherwise.
    pub fn convert_all<F>(self, mut op: F) -> Self
    where
        F: FnMut(Self) -> Self,
    {
        self.convert_all_inner(&mut op)
    }

    #[cfg_attr(not(debug_assertions), inline(always))]
    pub fn actual(&self) -> &Self {
        match self {
            Error::DebugContext(ctx) => ctx.inner.actual(),
            _ => self,
        }
    }

    fn convert_all_inner<F>(self, op: &mut F) -> Self
    where
        F: FnMut(Self) -> Self,
    {
        match self {
            Error::DebugContext(c) => {
                let c = c.convert_all(op);
                Error::DebugContext(c)
            }

            Error::Errors { span, errors } => {
                let mut new = Vec::with_capacity(errors.capacity());
                for err in errors {
                    new.push(err.convert_all_inner(op));
                }

                Error::Errors { span, errors: new }
            }

            _ => op(self),
        }
    }
}

impl DebugContext {
    fn convert<F>(self, op: F) -> Self
    where
        F: FnOnce(Error) -> Error,
    {
        let inner = box self.inner.convert(op);

        Self { inner, ..self }
    }

    fn convert_all<F>(self, op: &mut F) -> Self
    where
        F: FnMut(Error) -> Error,
    {
        let inner = box self.inner.convert_all_inner(op);

        Self { inner, ..self }
    }
}

#[derive(Clone, PartialEq, Spanned)]
pub struct DebugContext {
    pub span: Span,
    pub context: String,
    pub inner: Box<Error>,
}

impl Debug for DebugContext {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut next = Some(self);

        while let Some(cur) = next.take() {
            writeln!(f, "{}: {}", Yellow.paint("context"), cur.context)?;

            match &*cur.inner {
                Error::DebugContext(c) => next = Some(c),
                _ => {
                    Debug::fmt(&cur.inner, f)?;
                    break;
                }
            }
        }

        Ok(())
    }
}

impl Error {
    pub fn normalize_error_code(code: usize) -> usize {
        match code {
            // TS2304: Type not found.
            // TS2318: Type not found and name is global.
            // TS2552: Type not found with recommendation.
            // TS2580: Type not found with recommendation for package to instsall.
            // TS2581: Type not found with recommendation for jQuery.
            // TS2582: Type not found with recommendation for jest or mocha.
            // TS2583: Type not found with recommendation to change target library.
            // TS2584: Type not found with recommendation to change target library to include `dom`.
            2318 | 2552 | 2580 | 2581 | 2582 | 2583 | 2584 => 2304,

            // TS2339: Property not found.
            // TS2550: Property not found with a suggestion to change `lib`.
            // TS2551: Property not found with a suggestion.
            2550 | 2551 => 2339,

            // TS2693: Type used as a variable.
            // TS2585: Type used as a variable with a suggestion to change 'lib',
            2585 => 2693,

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

            _ => code,
        }
    }

    #[track_caller]
    pub fn context(self, context: impl Display) -> Self {
        if !cfg!(debug_assertions) {
            return self;
        }

        match self {
            Error::Errors { .. } | Error::DebugContext { .. } => {}
            _ => {
                if self.span().is_dummy() {
                    unreachable!("Error with dummy span found(context: {}): {:#?}", context, self)
                }
            }
        }

        Error::DebugContext(DebugContext {
            span: self.span(),
            context: context.to_string(),
            inner: box self,
        })
    }

    /// Split error into causes.
    pub fn into_causes(self) -> Vec<Self> {
        match self {
            Self::AssignFailed { cause, .. } => cause,
            Self::ObjectAssignFailed { errors, .. } => errors,
            Self::DebugContext(c) => {
                let DebugContext { span, context, .. } = c;

                c.inner
                    .into_causes()
                    .into_iter()
                    .map(|err| {
                        Error::DebugContext(DebugContext {
                            span,
                            inner: box err,
                            context: context.clone(),
                        })
                    })
                    .collect()
            }
            _ => {
                vec![self]
            }
        }
    }

    /// TypeScript error code.
    pub fn code(&self) -> usize {
        match self {
            Error::TS1016 { .. } => 1016,
            Error::TS1063 { .. } => 1063,
            Error::TS1094 { .. } => 1094,
            Error::TS1095 { .. } => 1095,
            Error::TS1168 { .. } => 1168,
            Error::TS1169 { .. } => 1169,
            Error::TS1183 { .. } => 1183,
            Error::TS1318 { .. } => 1318,
            Error::TS1319 { .. } => 1319,
            Error::ExportEqualsMixedWithOtherExports { .. } => 2309,
            Error::AnyTypeUsedAsCalleeWithTypeArgs { .. } => 2347,
            Error::TS2360 { .. } => 2360,
            Error::TS2361 { .. } => 2361,
            Error::WrongTypeForLhsOfNumericOperation { .. } => 2362,
            Error::WrongTypeForRhsOfNumericOperation { .. } => 2363,
            Error::TS2365 { .. } => 2365,
            Error::TS2370 { .. } => 2370,
            Error::WrongOverloadSignature { .. } => 2394,
            Error::TS1166 { .. } => 1166,
            Error::TS1345 { .. } => 1345,
            Error::TS2353 { .. } => 2353,
            Error::ConstructorImplMissingOrNotFollowedByDecl { .. } => 2390,
            Error::FnImplMissingOrNotFollowedByDecl { .. } => 2391,
            Error::InvalidTypeForComputedProperty { .. } => 2464,

            Error::ParamPropIsNotAllowedInAmbientConstructorx { .. } => 2369,
            Error::TS2389 { .. } => 2389,
            Error::TS2447 { .. } => 2447,
            Error::ClassDoesNotImplementMemeber { .. } => 2515,
            Error::TS2531 { .. } => 2531,
            Error::TS2567 { .. } => 2567,
            Error::TS2585 { .. } => 2585,
            Error::TS2704 { .. } => 2704,

            Error::AssignFailed { .. }
            | Error::AssignFailedDueToAccessibility { .. }
            | Error::ObjectAssignFailed { .. }
            | Error::SimpleAssignFailed { .. }
            | Error::SimpleAssignFailedWithCause { .. }
            | Error::InvalidAssignmentOfArray { .. }
            | Error::UnknownPropertyInObjectLiteralAssignment { .. }
            | Error::InvalidOpAssign { .. }
            | Error::TupleAssignError { .. } => 2322,

            Error::NonOverlappingTypeCast { .. } => 2352,

            Error::SuperInClassWithoutSuper { .. } => 2335,

            Error::NoSuchProperty { .. }
            | Error::NoSuchPropertyInThis { .. }
            | Error::NoSuchPropertyInClass { .. }
            | Error::NoSuchPropertyInModule { .. } => 2339,

            Error::AssignOpCannotBeApplied { .. } => 2365,
            Error::NonSymbolComputedPropInFormOfSymbol { .. } => 2471,
            Error::TypeUsedAsVar { .. } => 2693,
            Error::TypeNotFound { .. } => 2304,

            Error::NotVariable { .. } => 2539,

            Error::NoInitAndNoDefault { .. } => 2525,

            Error::ExpectedNArgsButGotM { .. } => 2554,
            Error::ExpectedAtLeastNArgsButGotM { .. } => 2555,
            Error::ExpectedNArgsButGotMOrMore { .. } => 2556,
            Error::ExpectedAtLeastNArgsButGotMOrMore { .. } => 2557,

            Error::ReferencedInInit { .. } => 2372,

            Error::InvalidDeleteOperand { .. } => 2703,

            Error::DuplicateName { .. } | Error::DuplicateNameWithoutName { .. } => 2300,

            Error::NoSuchVar { .. } => 2304,
            Error::NoSuchType { .. } => 2304,
            Error::NoSuchTypeButVarExists { .. } => 2749,
            Error::NoSuchVarButThisHasSuchProperty { .. } => 2663,

            Error::CannotAssignAbstractConstructorToNonAbstractConstructor { .. } => 2322,
            Error::CannotCreateInstanceOfAbstractClass { .. } => 2511,
            Error::WrongArgType { .. } => 2345,

            Error::ComputedMemberInEnumWithStrMember { .. } => 2553,

            Error::TupleIndexError { .. } => 2493,
            Error::InvalidLValue { .. } => 2540,

            Error::TS2378 { .. } => 2378,

            Error::ConstEnumNonIndexAccess { .. } => 2476,

            Error::InvalidUseOfConstEnum { .. } => 2475,

            Error::DebugContext(c) => c.inner.code(),

            Error::ObjectIsPossiblyNull { .. } => 2531,
            Error::ObjectIsPossiblyUndefined { .. } | Error::ObjectIsPossiblyUndefinedWithType { .. } => 2532,
            Error::ObjectIsPossiblyNullOrUndefined { .. } => 2533,

            Error::InvalidBinaryOp { .. } => 2365,

            Error::CannotCompareWithOp { .. } => 2365,

            Error::TypeInvalidForUpdateArg { .. } => 2356,
            Error::ExprInvalidForUpdateArg { .. } => 2357,

            Error::CannotAssignToNonVariable { .. } => 2539,
            Error::CannotAssignToFunction { .. } => 2630,

            Error::AssignedWrapperToPrimitive { .. } => 2322,

            Error::AccessibilityDiffers { .. } => 2322,

            Error::InvalidInitInConstEnum { .. } => 2474,

            Error::InvalidTupleCast { .. } => 2352,

            Error::NoOverlap { .. } => 2367,

            Error::InvalidLhsInInstanceOf { .. } => 2358,

            Error::InvalidRhsInInstanceOf { .. } => 2359,

            Error::NumericUnaryOpToSymbol { .. } => 2469,

            Error::InvalidNumericOperand { .. } => 2356,

            Error::UpdateOpToSymbol { .. } => 2469,

            Error::UselessSeqExpr { .. } => 2695,

            Error::EnumCannotBeLValue { .. } => 2540,

            Error::NoSuchEnumVariant { .. } => 2339,

            Error::TupleTooShort { .. } => 2339,

            Error::SwitchCaseTestNotCompatible { .. } => 2678,

            Error::NullishCoalescingMixedWithLogicalWithoutParen { .. } => 5076,

            Error::OptionalBindingPatternInImplSignature { .. } => 2463,

            Error::GeneratorCannotHaveVoidAsReturnType { .. } => 2505,

            Error::MissingFields { .. } => 2741,

            Error::MustHaveSymbolIteratorThatReturnsIterator { .. } => 2488,

            Error::MustHaveSymbolAsycIteratorThatReturnsIterator { .. } => 2504,

            Error::MustHaveSymbolIteratorThatReturnsIteratorOrMustBeArray { .. } => 2548,

            Error::NoSuchPropertyWhileDeclWithBidningPat { .. } => 2525,

            Error::NoNewSignature { .. } => 2351,

            Error::Unknown { .. } => 2571,

            Error::ReturnRequired { .. } => 2355,

            Error::ThisRefToModuleOrNamespace { .. } => 2331,

            Error::CannotReferenceThisInComputedPropName { .. } => 2465,
            Error::CannotReferenceSuperInComputedPropName { .. } => 2466,
            Error::DeclaringTypeParamReferencedByComputedPropName { .. } => 2467,

            Error::StaticMemberCannotUseTypeParamOfClass { .. } => 2302,

            Error::InvalidImplOfInterface { .. } => 2420,

            Error::ClassIncorrectlyImplementsInterface { .. } => 2420,

            Error::ExportMixedWithLocal { .. } => 2395,

            Error::NotConstructorType { .. } => 2507,

            Error::SelfReferentialSuperClass { .. } => 2506,

            Error::StaticPropertyCannotBeNamedPrototype { .. } => 2699,

            Error::CannotExportNonLocalVar { .. } => 2661,

            Error::DuplicateProperty { .. } => 2300,

            Error::CannotCallWithNewNonVoidFunction { .. } => 2350,

            Error::InvalidInterfaceName { .. } => 2427,

            Error::InvalidUseOfArgumentsInEs3OrEs5 { .. } => 2496,

            Error::ArgumentsCannotBeUsedInAsyncFnInEs3OrEs5 { .. } => 2522,

            Error::NoMatchingOverload { .. } => 2769,

            Error::NoSuchVarForShorthand { .. } => 18004,

            Error::NoCallSignature { .. } => 2349,

            Error::InvalidClassName { .. } => 2414,

            Error::InitializerDisallowedInAmbientContext { .. } => 2371,

            Error::ImcompatibleFnOverload { .. } => 2394,

            Error::ImplicitReturnType { .. } => 7010,

            Error::InvalidLhsOfAssign { .. } => 2364,

            Error::EnumMemberIdCannotBeNumber { .. } => 2452,

            Error::NamspaceNotFound { .. } => 2503,

            Error::WithStmtNotSupported { .. } => 2410,

            Error::InvalidSuperClass { .. } => 2507,

            Error::ThisInConstructorParam { .. } => 2333,

            Error::ThisInStaticPropertyInitializer { .. } => 2334,

            Error::ImplicitAny { .. } => 7005,

            Error::ImplicitAnyBecauseNoIndexSignatureExists { .. } => 7052,

            Error::ImplicitAnyBecauseIndexTypeIsWrong { .. } => 7053,

            Error::ImplicitAnyBecauseOfSelfRef { .. } => 7022,

            Error::ConstructorIsKeyword { .. } => 18012,

            Error::PrivateIdUsedAsMethodName { .. } => 18022,

            Error::CannotDeletePrivateProperty { .. } => 18011,

            Error::CannotAccessPrivatePropertyFromOutside { .. } => 18013,

            Error::OptionalChainCannotContainPrivateIdentifier { .. } => 18030,

            Error::TypeAnnOnLhsOfForInLoops { .. } => 2404,
            Error::TypeAnnOnLhsOfForOfLoops { .. } => 2483,

            Error::DestructuringBindingNotAllowedInLhsOfForIn { .. } => 2491,

            Error::WrongTypeForLhsOfForInLoop { .. } => 2405,

            Error::InvalidExprOfLhsOfForIn { .. } => 2406,
            Error::InvalidExprOfLhsOfForOf { .. } => 2487,

            Error::LetOrConstIsNotValidIdInLetOrConstVarDecls { .. } => 2480,
            Error::ForOfStringUsedInEs3 { .. } => 2494,

            Error::NotArrayType { .. } => 2461,
            Error::NotArrayTypeNorStringType { .. } => 2495,
            Error::NotArrayTypeNorStringTypeButDownlevelIterationWouldWork { .. } => 2569,

            Error::NoCallablePropertyWithName { .. } => 2349,

            Error::NoMethodNamedNext { .. } => 2489,

            Error::NotGeneric { .. } => 2315,

            Error::CannotAssignToReadonlyProperty { .. } => 2540,

            Error::ReadOnly { .. } => 2546,

            Error::ClassNameCannotBeObjectWhenTargetingEs5WithModule { .. } => 2725,

            Error::DuplicateVar { .. } => 2451,

            Error::TooManyAsterisk { .. } => 5061,

            Error::ModuleNotFound { .. } => 2307,

            Error::DuplicateConstructor { .. } => 2392,

            Error::DuplicateFnImpl { .. } => 2393,

            Error::DuplicateDefaultExport { .. } => 2528,

            Error::BlockScopedVarUsedBeforeInit { .. } => 2448,

            Error::SuperCannotUseTypeArgs { .. } => 2754,

            Error::DeleteOperandMustBeOptional { .. } => 2790,

            Error::BindingPatNotAllowedInRestPatArg { .. } => 2501,

            Error::RestArgMustBeVarOrMemberAccess { .. } => 2701,

            Error::CannotAssignToNamespace { .. } => 2631,

            Error::ReturnPropertyOfIteratorMustBeMethod { .. } => 2767,

            Error::NextOfItertorShouldReturnTypeWithPropertyValue { .. } => 2490,

            Error::InvalidUsageOfNewTarget { .. } => 17013,

            Error::AssignFailedBecauseTupleLengthDiffers { .. } => 2322,

            Error::ClassMemberNotCompatibleWithStringIndexSignature { .. } => 2411,

            Error::ClassMemberNotCompatibleWithNumericIndexSignature { .. } => 2412,

            Error::AbstractAndConcreteIsMixed { .. } => 2512,

            Error::AbstractClassMethodShouldBeSequntial { .. } => 2516,

            Error::OperatorCannotBeAppliedToTypes { .. } => 2365,

            Error::CannotAccessAbstractMember { .. } => 2513,

            Error::SuperNotCalled { .. } => 2377,

            Error::SuperInNestedFunction { .. } => 2337,

            Error::InvalidOperandOfIncDecOptionalProp { .. } => 2777,

            Error::InvalidRestPatternInOptionalChain { .. } => 2778,

            Error::InvalidLhsOfAssignOptionalProp { .. } => 2779,

            Error::InvalidRestPatternInForIn { .. } => 2780,

            Error::InvalidRestPatternInForOf { .. } => 2781,

            Error::ThisUsedBeforeCallingSuper { .. } => 17009,

            Error::SuperUsedBeforeCallingSuper { .. } => 17011,

            Error::OptionInvalidForEs3 { .. } => 5048,

            Error::ShouldBeStaticMethod { .. } => 2387,

            Error::ShouldBeInstanceMethod { .. } => 2388,

            Error::DefinedWitHAccessorInSuper { .. } => 2610,

            Error::ClassPropNotInitialized { .. } => 2564,

            Error::VarMayNotBeInitialized { .. } => 2454,

            Error::UnreachableCode { .. } => 7027,

            Error::ConstEnumMemberHasInifinityAsInit { .. } => 2477,

            Error::ConstEnumMemberHasNaNAsInit { .. } => 2478,

            Error::OnlyOneEnumCanOmitInit { .. } => 2432,

            Error::CannotUseTypeAsIndexIndex { .. } => 2538,

            Error::InterfaceNotCompatible { .. } => 2320,

            Error::UpdateArgMustBeVariableOrPropertyAccess { .. } => 2357,

            Error::OptionalAndNonOptionalMethodPropertyMixed { .. } => 2386,

            Error::TypeParamsProvidedButCalleeIsNotGeneric { .. } => 2347,

            Error::IntrinsicIsBuiltinOnly { .. } => 2795,

            Error::VarDeclNotCompatible { .. } => 2403,

            Error::InvalidInterfaceInheritance { .. } => 2430,

            Error::TargetLacksConstructSignature { .. } => 7009,

            _ => 0,
        }
    }

    pub fn is_property_not_found(&self) -> bool {
        match self.actual() {
            Error::NoSuchProperty { .. }
            | Error::NoSuchPropertyInClass { .. }
            | Error::NoSuchPropertyInModule { .. }
            | Error::NoSuchPropertyInThis { .. } => true,
            _ => false,
        }
    }

    pub fn is_var_not_found(&self) -> bool {
        match self.actual() {
            Self::NoSuchVar { .. } | Self::NoSuchVarButThisHasSuchProperty { .. } | Self::NoSuchVarForShorthand { .. } => true,
            _ => false,
        }
    }

    pub fn is_assign_failure(&self) -> bool {
        self.code() == 2322
    }

    pub fn is_type_not_found(&self) -> bool {
        match self.actual() {
            Self::NoSuchType { .. } | Self::NoSuchTypeButVarExists { .. } => true,
            _ => false,
        }
    }

    fn msg(&self) -> Cow<'static, str> {
        match self {
            Self::Unimplemented { msg, .. } => format!("unimplemented: {}", msg).into(),

            _ => format!("{:#?}", self).into(),
        }
    }

    #[cold]
    pub fn emit(self, h: &Handler) {
        let span = self.span();

        let mut err = h.struct_span_err_with_code(
            span,
            &self.msg(),
            DiagnosticId::Error(format!("TS{}", Self::normalize_error_code(self.code()))),
        );

        err.emit();
    }

    #[cold]
    pub fn flatten(vec: Vec<Error>) -> Vec<Error> {
        let mut buf = Vec::with_capacity(vec.len());

        for e in vec {
            match e {
                Error::Errors { errors, .. } | Error::TupleAssignError { errors, .. } => buf.extend(Self::flatten(errors)),
                Error::DebugContext(DebugContext { inner, context, .. }) => {
                    //
                    buf.extend(Self::flatten(vec![*inner]).into_iter().map(|inner| {
                        Error::DebugContext(DebugContext {
                            span: inner.span(),
                            context: context.clone(),
                            inner: box inner,
                        })
                    }))
                }
                _ => buf.push(e),
            }
        }

        buf
    }
}

impl From<Vec<Error>> for Error {
    #[inline]
    fn from(errors: Vec<Error>) -> Self {
        Error::Errors { span: DUMMY_SP, errors }
    }
}

impl From<Errors> for Error {
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

impl From<StackOverflowError> for Error {
    fn from(e: StackOverflowError) -> Self {
        Error::StackOverflow { span: e.span }
    }
}
