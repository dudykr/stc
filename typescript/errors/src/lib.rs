#![allow(incomplete_features)]
#![deny(variant_size_differences)]
#![feature(box_syntax)]
#![feature(specialization)]

pub use self::result_ext::DebugExt;
use fmt::Formatter;
use stc_ts_types::name::Name;
use stc_ts_types::Id;
use stc_ts_types::Key;
use stc_ts_types::ModuleId;
use stc_ts_types::Type;
use stc_ts_types::TypeElement;
use stc_ts_types::TypeParamInstantiation;
use std::borrow::Cow;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::{ops::RangeInclusive, path::PathBuf};
use swc_atoms::JsWord;
use swc_common::errors::DiagnosticId;
use swc_common::{errors::Handler, Span, Spanned, DUMMY_SP};
use swc_ecma_ast::AssignOp;
use swc_ecma_ast::BinaryOp;
use swc_ecma_ast::{UnaryOp, UpdateOp};

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

        if err.span().is_dummy() {
            panic!("Error with a dummy span found: {:?}", err)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub enum Error {
    GeneratorCannotHaveVoidAsReturnType {
        span: Span,
    },

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

    SwitchCaseTestNotCompatible {
        span: Span,
    },

    EnumCannotBeLValue {
        span: Span,
    },

    ExprInvalidForUpdateArg {
        span: Span,
    },

    TypeInvalidForUpdateArg {
        span: Span,
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
    },

    InvalidBinaryOp {
        span: Span,
        op: BinaryOp,
    },

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
    },

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

    ReadOnly {
        span: Span,
    },

    ImplicitAny {
        span: Span,
    },

    TupleAssignError {
        span: Span,
        errors: Vec<Box<Error>>,
    },

    Errors {
        span: Span,
        errors: Vec<Box<Error>>,
    },

    RedeclaredVarWithDifferentType {
        span: Span,
    },

    NoSuchType {
        span: Span,
        name: Id,
    },

    NoSuchVar {
        span: Span,
        name: Id,
    },

    TypeUsedAsVar {
        span: Span,
        name: Id,
    },

    DuplicateName {
        name: Id,
        span: Span,
    },

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

    TooManyTupleElements {
        span: Span,
    },

    NotTuple {
        span: Span,
    },

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
        left: Box<Type>,
        right: Box<Type>,
        cause: Vec<Box<Error>>,
    },

    ObjectAssignFailed {
        span: Span,
        errors: Vec<Box<Error>>,
    },

    SimpleAssignFailed {
        span: Span,
    },

    InvalidAssignmentOfArray {
        span: Span,
    },

    /// a or b or c
    UnionError {
        span: Span,
        errors: Vec<Box<Error>>,
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

    NoNewSignature {
        span: Span,
        callee: Box<Type>,
    },

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

    TS2309 {
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

    TS2362 {
        span: Span,
    },

    TS2363 {
        span: Span,
    },

    TS2365 {
        span: Span,
    },

    TS2370 {
        span: Span,
    },

    TS2394 {
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

    FnImplMissingOrNotFollowedByDecl {
        span: Span,
    },

    TS2464 {
        span: Span,
    },

    TS2356 {
        span: Span,
    },

    TS2369 {
        span: Span,
    },

    TS2389 {
        span: Span,
    },

    TS2447 {
        span: Span,
    },

    TS2515 {
        span: Span,
    },

    TS2531 {
        span: Span,
    },

    TS2532 {
        span: Span,
    },

    TS2567 {
        span: Span,
    },

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

    InvalidDeleteOperand {
        span: Span,
    },

    NoCallabelPropertyWithName {
        span: Span,
        key: Box<Key>,
    },

    MustHaveSymbolIteratorThatReturnsIterator {
        span: Span,
    },

    NoSuchConstructor {
        span: Span,
        key: Box<Key>,
    },

    DebugContext(DebugContext),
}

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
                    new.push(box err.convert_all_inner(op));
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
            writeln!(f, "context: {}", cur.context)?;

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
    #[track_caller]
    pub fn context(self, context: impl Display) -> Box<Self> {
        if !cfg!(debug_assertions) {
            return box self;
        }

        match self {
            Error::Errors { .. } | Error::DebugContext { .. } => {}
            _ => {
                if self.span().is_dummy() {
                    panic!("Error with dummy span found(context: {}): {:#?}", context, self)
                }
            }
        }

        box Error::DebugContext(DebugContext {
            span: self.span(),
            context: context.to_string(),
            inner: box self,
        })
    }

    /// TypeScript error code.
    fn code(&self) -> usize {
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
            Error::TS2309 { .. } => 2309,
            Error::AnyTypeUsedAsCalleeWithTypeArgs { .. } => 2347,
            Error::TS2360 { .. } => 2360,
            Error::TS2361 { .. } => 2361,
            Error::TS2362 { .. } => 2362,
            Error::TS2363 { .. } => 2363,
            Error::TS2365 { .. } => 2365,
            Error::TS2370 { .. } => 2370,
            Error::TS2394 { .. } => 2394,
            Error::TS1166 { .. } => 1166,
            Error::TS1345 { .. } => 1345,
            Error::TS2353 { .. } => 2353,
            Error::FnImplMissingOrNotFollowedByDecl { .. } => 2391,
            Error::TS2464 { .. } => 2464,
            Error::TS2356 { .. } => 2356,
            Error::TS2369 { .. } => 2369,
            Error::TS2389 { .. } => 2389,
            Error::TS2447 { .. } => 2447,
            Error::TS2515 { .. } => 2515,
            Error::TS2531 { .. } => 2531,
            Error::TS2532 { .. } => 2532,
            Error::TS2567 { .. } => 2567,
            Error::TS2585 { .. } => 2585,
            Error::TS2704 { .. } => 2704,

            Error::AssignFailed { .. }
            | Error::ObjectAssignFailed { .. }
            | Error::SimpleAssignFailed { .. }
            | Error::InvalidAssignmentOfArray { .. }
            | Error::UnknownPropertyInObjectLiteralAssignment { .. }
            | Error::InvalidOpAssign { .. }
            | Error::TupleAssignError { .. } => 2322,

            Error::NonOverlappingTypeCast { .. } => 2352,

            Error::NoSuchProperty { .. } => 2339,
            Error::AssignOpCannotBeApplied { .. } => 2365,
            Error::NonSymbolComputedPropInFormOfSymbol { .. } => 2471,
            Error::TypeUsedAsVar { .. } => 2585,
            Error::TypeNotFound { .. } => 2304,

            Error::ExpectedNArgsButGotM { .. } => 2554,
            Error::ExpectedAtLeastNArgsButGotM { .. } => 2555,
            Error::ExpectedNArgsButGotMOrMore { .. } => 2556,
            Error::ExpectedAtLeastNArgsButGotMOrMore { .. } => 2557,

            Error::ReferencedInInit { .. } => 2372,

            Error::InvalidDeleteOperand { .. } => 2703,
            Error::NoSuchVar { .. } => 2304,
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
            Error::ObjectIsPossiblyUndefined { .. } => 2532,

            Error::InvalidBinaryOp { .. } => 2365,

            Error::CannotCompareWithOp { .. } => 2365,

            Error::TypeInvalidForUpdateArg { .. } => 2356,
            Error::ExprInvalidForUpdateArg { .. } => 2357,

            Error::CannotAssignToNonVariable { .. } => 2539,

            Error::AssignedWrapperToPrimitive { .. } => 2322,

            Error::AccessibilityDiffers { .. } => 2322,

            Error::InvalidInitInConstEnum { .. } => 2474,

            Error::InvalidTupleCast { .. } => 2352,

            Error::NoOverlap { .. } => 2367,

            Error::InvalidLhsInInstanceOf { .. } => 2358,

            Error::InvalidRhsInInstanceOf { .. } => 2359,

            Error::NumericUnaryOpToSymbol { .. } => 2469,

            Error::UpdateOpToSymbol { .. } => 2469,

            Error::UselessSeqExpr { .. } => 2695,

            Error::EnumCannotBeLValue { .. } => 2540,

            Error::NoSuchEnumVariant { .. } => 2339,

            Error::SwitchCaseTestNotCompatible { .. } => 2678,

            Error::NullishCoalescingMixedWithLogicalWithoutParen { .. } => 5076,

            Error::OptionalBindingPatternInImplSignature { .. } => 2463,

            Error::GeneratorCannotHaveVoidAsReturnType { .. } => 2505,

            Error::MissingFields { .. } => 2741,

            Error::MustHaveSymbolIteratorThatReturnsIterator { .. } => 2488,

            Error::NoSuchPropertyWhileDeclWithBidningPat { .. } => 2525,

            Error::NoNewSignature { .. } => 2555,

            Error::Unknown { .. } => 2571,

            Error::ReturnRequired { .. } => 2355,

            _ => 0,
        }
    }

    fn msg(&self) -> Cow<'static, str> {
        match self {
            Self::FnImplMissingOrNotFollowedByDecl { .. } => {
                "Function implementation is missing or not immediately following the declaration".into()
            }
            Self::NonOverlappingTypeCast { .. } => "Conversion of type may be a mistake because neither type \
                                                    sufficiently overlaps with the other. If this was intentional, \
                                                    convert the expression to 'unknown' first."
                .into(),

            Self::AssignOpCannotBeApplied { op, .. } => format!("Operator '{}' cannot be applied to types", op).into(),

            Self::NonSymbolComputedPropInFormOfSymbol { .. } => {
                "A computed property name of the form '{TODO}' must be of type 'symbol'.".into()
            }

            Self::Unimplemented { msg, .. } => format!("unimplemented: {}", msg).into(),

            _ => format!("{:#?}", self).into(),
        }
    }

    #[cold]
    pub fn emit(self, h: &Handler) {
        let span = self.span();

        let mut err = h.struct_span_err_with_code(span, &self.msg(), DiagnosticId::Error(format!("TS{}", self.code())));

        err.emit();
    }

    #[cold]
    pub fn flatten(vec: Vec<Box<Error>>) -> Vec<Box<Error>> {
        let mut buf = Vec::with_capacity(vec.len());

        for e in vec {
            match *e {
                Error::Errors { errors, .. } | Error::TupleAssignError { errors, .. } => {
                    buf.extend(Self::flatten(errors))
                }
                Error::DebugContext(DebugContext { inner, context, .. }) => {
                    //
                    buf.extend(Self::flatten(vec![inner]).into_iter().map(|inner| {
                        box Error::DebugContext(DebugContext {
                            span: inner.span(),
                            context: context.clone(),
                            inner,
                        })
                    }))
                }
                _ => buf.push(e),
            }
        }

        buf
    }
}

impl From<Vec<Box<Error>>> for Error {
    #[inline]
    fn from(errors: Vec<Box<Error>>) -> Self {
        Error::Errors { span: DUMMY_SP, errors }
    }
}

impl From<Errors> for Error {
    #[inline]
    fn from(errors: Errors) -> Self {
        errors.0.into()
    }
}

impl From<Errors> for Box<Error> {
    #[inline]
    fn from(errors: Errors) -> Self {
        box errors.into()
    }
}

/// A utility type to track
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Errors(Vec<Box<Error>>);

impl From<Errors> for Vec<Box<Error>> {
    #[inline]
    fn from(e: Errors) -> Self {
        e.0
    }
}

impl IntoIterator for Errors {
    type Item = Box<Error>;
    type IntoIter = <Vec<Box<Error>> as IntoIterator>::IntoIter;

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
    pub fn push(&mut self, err: Box<Error>) {
        self.validate(&err);

        self.0.push(err);
    }

    #[inline]
    pub fn reserve(&mut self, v: usize) {
        self.0.reserve(v)
    }

    #[inline]
    pub fn append(&mut self, other: &mut Vec<Box<Error>>) {
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

impl Extend<Box<Error>> for Errors {
    #[inline]
    fn extend<T: IntoIterator<Item = Box<Error>>>(&mut self, iter: T) {
        if cfg!(debug_assertions) {
            for err in iter {
                self.push(err)
            }
        } else {
            self.0.extend(iter)
        }
    }
}
