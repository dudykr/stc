//! This crate exists to reduce compile time.
//!
//! The visitor is too slow to compile every time I make change.
#![deny(deprecated)]
#![allow(incomplete_features)]
#![allow(clippy::needless_update)]
#![feature(adt_const_params)]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(specialization)]

use std::{
    self,
    borrow::Cow,
    fmt,
    fmt::{Debug, Formatter},
    iter::FusedIterator,
    mem::{replace, transmute},
    ops::AddAssign,
};

use fxhash::FxHashMap;
use is_macro::Is;
use num_bigint::BigInt;
use num_traits::Zero;
use rnode::{FoldWith, VisitMut, VisitMutWith, VisitWith};
use scoped_tls::scoped_thread_local;
use serde::{Deserialize, Serialize};
use static_assertions::assert_eq_size;
use stc_arc_cow::{freeze::Freezer, ArcCow};
use stc_ts_ast_rnode::{
    RBigInt, RExpr, RNumber, RPat, RPrivateName, RStr, RTplElement, RTsEntityName, RTsEnumMemberId, RTsLit, RTsModuleName,
    RTsThisTypeOrIdent,
};
use stc_utils::{
    cache::{Freeze, ALLOW_DEEP_CLONE},
    ext::TypeVecExt,
    panic_ctx,
};
use stc_visit::{Visit, Visitable};
use swc_atoms::{js_word, Atom, JsWord};
use swc_common::{util::take::Take, EqIgnoreSpan, FromVariant, Span, Spanned, SyntaxContext, TypeEq, DUMMY_SP};
use swc_ecma_ast::{Accessibility, TruePlusMinus, TsKeywordTypeKind};
use swc_ecma_utils::{
    Value,
    Value::{Known, Unknown},
};
use tracing::instrument;
use tracker::Tracker;
use triomphe::Arc;

use self::type_id::SymbolId;
pub use self::{
    convert::rprop_name_to_expr,
    id::Id,
    intrinsic::{IntrinsicKind, StringMapping},
    metadata::*,
    module_id::ModuleId,
};

mod convert;
mod id;
mod intrinsic;
mod is;
pub mod macros;
mod metadata;
pub mod module_id;
pub mod name;
pub mod replace;
mod tracker;
pub mod type_id;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IdCtx {
    Var,
    Type,
}

#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct ModuleTypeData {
    pub private_vars: FxHashMap<Id, Type>,
    pub vars: FxHashMap<JsWord, Type>,

    pub private_types: FxHashMap<Id, Vec<Type>>,
    pub types: FxHashMap<JsWord, Vec<Type>>,
}

impl Visitable for ModuleTypeData {}

impl<V: ?Sized> VisitWith<V> for ModuleTypeData {
    fn visit_children_with(&self, _: &mut V) {}
}

impl<V: ?Sized> VisitMutWith<V> for ModuleTypeData {
    fn visit_mut_children_with(&mut self, _: &mut V) {}
}

impl<V: ?Sized> FoldWith<V> for ModuleTypeData {
    fn fold_children_with(self, _: &mut V) -> Self {
        self
    }
}

impl TypeEq for ModuleTypeData {
    #[inline]
    fn type_eq(&self, _: &Self) -> bool {
        false
    }
}

impl EqIgnoreSpan for ModuleTypeData {
    #[inline]
    fn eq_ignore_span(&self, _: &Self) -> bool {
        false
    }
}

impl AddAssign for ModuleTypeData {
    fn add_assign(&mut self, other: Self) {
        self.types.extend(other.types);
        self.private_types.extend(other.private_types);

        self.vars.extend(other.vars);
        self.private_vars.extend(other.private_vars);
    }
}

/// A TypeScript type.
///
/// # Invariants
///
///  - [Type::Union] cannot have a single element.
///  - [Type::Intersection] cannot have a single element.
///
///  - [Type::Union] cannot have [Type::Union] as a element,
///  - [Type::Intersection] cannot have [Type::Intersection] as a element,
///
/// [`Type::assert_valid`] can be used to ensure invariants.
/// Note that this is noop in release build.
///
/// # Clone
///
/// To reduce memory usage, this type should be `freeze()`-ed.
/// Freezed type is immutable, and it's cheap to clone.
/// When required, you can use `normalize_mut()` or `foldable()` to get mutable
/// version.
///
/// **Note**: You have to call `normalize()` while pattern matching.
///
/// To enforce this, deep cloning is not allowed by default. If you want to
/// clone deeply, you have to clone this type in a closure passed to
/// [`ALLOW_DEEP_CLONE`]. But this is not recommended, and should be avoided for
/// performance.
#[derive(Debug, PartialEq, Spanned, FromVariant, EqIgnoreSpan, Visit, Serialize, Deserialize)]
pub enum Type {
    Instance(Instance),
    StaticThis(StaticThis),
    This(ThisType),
    Lit(LitType),
    Query(QueryType),
    Infer(InferType),
    Import(ImportType),
    Predicate(Predicate),
    IndexedAccessType(IndexedAccessType),

    Ref(Ref),
    TypeLit(TypeLit),
    Keyword(KeywordType),
    Conditional(Conditional),
    Tuple(Tuple),
    Array(Array),
    Union(Union),
    Intersection(Intersection),
    Function(Function),
    Constructor(Constructor),

    Index(Index),
    Readonly(Readonly),
    Unique(Unique),

    Param(TypeParam),
    EnumVariant(EnumVariant),

    Interface(Interface),

    Enum(ArcCow<Enum>),

    Mapped(Mapped),

    /// export type A<B> = Foo<B>;
    Alias(Alias),
    Namespace(Namespace),
    Module(Module),

    /// Instance of a class.
    Class(Class),

    /// Class definition itself.
    ClassDef(ArcCow<ClassDef>),

    Arc(Freezed),

    Rest(RestType),

    Optional(OptionalType),

    Symbol(Symbol),

    Tpl(TplType),

    StringMapping(StringMapping),
}

impl Clone for Type {
    #[instrument(name = "Type::clone", skip_all)]
    fn clone(&self) -> Self {
        match self {
            Type::Arc(ty) => ty.clone().into(),
            Type::Keyword(ty) => ty.clone().into(),
            Type::StaticThis(ty) => ty.clone().into(),
            Type::This(ty) => ty.clone().into(),
            Type::Symbol(ty) => (*ty).into(),

            _ => {
                scoped_thread_local!(static DEEP: ());

                macro_rules! work {
                    () => {{
                        match self {
                            Type::StringMapping(ty) => ty.clone().into(),
                            Type::Instance(ty) => ty.clone().into(),
                            Type::Lit(ty) => ty.clone().into(),
                            Type::Query(ty) => ty.clone().into(),
                            Type::Infer(ty) => ty.clone().into(),
                            Type::Import(ty) => ty.clone().into(),
                            Type::Predicate(ty) => ty.clone().into(),
                            Type::IndexedAccessType(ty) => ty.clone().into(),
                            Type::Ref(ty) => ty.clone().into(),
                            Type::TypeLit(ty) => ty.clone().into(),
                            Type::Conditional(ty) => ty.clone().into(),
                            Type::Tuple(ty) => ty.clone().into(),
                            Type::Array(ty) => ty.clone().into(),
                            Type::Union(ty) => ty.clone().into(),
                            Type::Intersection(ty) => ty.clone().into(),
                            Type::Function(ty) => ty.clone().into(),
                            Type::Constructor(ty) => ty.clone().into(),
                            Type::Index(ty) => ty.clone().into(),
                            Type::Readonly(ty) => ty.clone().into(),
                            Type::Unique(ty) => ty.clone().into(),
                            Type::Param(ty) => ty.clone().into(),
                            Type::EnumVariant(ty) => ty.clone().into(),
                            Type::Interface(ty) => ty.clone().into(),
                            Type::Enum(ty) => ty.clone().into(),
                            Type::Mapped(ty) => ty.clone().into(),
                            Type::Alias(ty) => ty.clone().into(),
                            Type::Namespace(ty) => ty.clone().into(),
                            Type::Module(ty) => ty.clone().into(),
                            Type::Class(ty) => ty.clone().into(),
                            Type::ClassDef(ty) => ty.clone().into(),
                            Type::Rest(ty) => ty.clone().into(),
                            Type::Optional(ty) => ty.clone().into(),
                            Type::Tpl(ty) => ty.clone().into(),
                            _ => {
                                unreachable!()
                            }
                        }
                    }};
                }

                if cfg!(debug_assertions) && self.span() != DUMMY_SP && !self.is_clone_cheap() && !ALLOW_DEEP_CLONE.is_set() {
                    let _panic_ctx = panic_ctx!(format!("{:?}", self));

                    if DEEP.is_set() {
                        unreachable!("Deep clone of type is not allowed")
                    }

                    DEEP.set(&(), || work!())
                } else {
                    work!()
                }
            }
        }
    }
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(Type, [u8; 112]);

impl TypeEq for Type {
    fn type_eq(&self, other: &Self) -> bool {
        match (self.normalize(), other.normalize()) {
            (Type::Instance(l), Type::Instance(r)) => l.type_eq(r),
            (Type::StaticThis(l), Type::StaticThis(r)) => l.type_eq(r),
            (Type::This(l), Type::This(r)) => l.type_eq(r),
            (Type::Lit(l), Type::Lit(r)) => l.type_eq(r),
            (Type::Query(l), Type::Query(r)) => l.type_eq(r),
            (Type::Infer(l), Type::Infer(r)) => l.type_eq(r),
            (Type::Import(l), Type::Import(r)) => l.type_eq(r),
            (Type::Predicate(l), Type::Predicate(r)) => l.type_eq(r),
            (Type::IndexedAccessType(l), Type::IndexedAccessType(r)) => l.type_eq(r),
            (Type::Ref(l), Type::Ref(r)) => l.type_eq(r),
            (Type::TypeLit(l), Type::TypeLit(r)) => l.type_eq(r),
            (Type::Keyword(l), Type::Keyword(r)) => l.type_eq(r),
            (Type::Conditional(l), Type::Conditional(r)) => l.type_eq(r),
            (Type::Tuple(l), Type::Tuple(r)) => l.type_eq(r),
            (Type::Array(l), Type::Array(r)) => l.type_eq(r),
            (Type::Union(l), Type::Union(r)) => l.type_eq(r),
            (Type::Intersection(l), Type::Intersection(r)) => l.type_eq(r),
            (Type::Function(l), Type::Function(r)) => l.type_eq(r),
            (Type::Constructor(l), Type::Constructor(r)) => l.type_eq(r),
            (Type::Index(l), Type::Index(r)) => l.type_eq(r),
            (Type::Readonly(l), Type::Readonly(r)) => l.type_eq(r),
            (Type::Unique(l), Type::Unique(r)) => l.type_eq(r),
            (Type::Param(l), Type::Param(r)) => l.type_eq(r),
            (Type::EnumVariant(l), Type::EnumVariant(r)) => l.type_eq(r),
            (Type::Interface(l), Type::Interface(r)) => l.type_eq(r),
            (Type::Enum(l), Type::Enum(r)) => l.type_eq(r),
            (Type::Mapped(l), Type::Mapped(r)) => l.type_eq(r),
            (Type::Alias(l), Type::Alias(r)) => l.type_eq(r),
            (Type::Namespace(l), Type::Namespace(r)) => l.type_eq(r),
            (Type::Module(l), Type::Module(r)) => l.type_eq(r),
            (Type::Class(l), Type::Class(r)) => l.type_eq(r),
            (Type::ClassDef(l), Type::ClassDef(r)) => l.type_eq(r),
            (Type::Rest(l), Type::Rest(r)) => l.type_eq(r),
            (Type::Optional(l), Type::Optional(r)) => l.type_eq(r),
            (Type::Symbol(l), Type::Symbol(r)) => l.type_eq(r),
            (Type::StringMapping(l), Type::StringMapping(r)) => l.type_eq(r),
            (Type::Tpl(l), Type::Tpl(r)) => l.type_eq(r),
            _ => false,
        }
    }
}

fn _assert_send_sync() {
    fn assert<T: Send + Sync>() {}

    assert::<Type>();
}

#[derive(Clone, PartialEq, EqIgnoreSpan, Visit, Is, Spanned, Serialize, Deserialize)]
pub enum Key {
    Computed(ComputedKey),
    Normal { span: Span, sym: JsWord },
    Num(RNumber),
    BigInt(RBigInt),
    Private(PrivateName),
}

impl Debug for Key {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Key::Computed(v) => v.fmt(f),
            Key::Normal { sym, .. } => write!(f, "{}", sym),
            Key::Num(v) => write!(f, "{}", v.value),
            Key::BigInt(v) => write!(f, "{}", v.value),
            Key::Private(v) => v.fmt(f),
        }
    }
}

impl TypeEq for Key {
    fn type_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Key::Normal { sym: l, .. }, Key::Normal { sym: r, .. }) => l == r,
            (Key::Num(l), Key::Num(r)) => l.type_eq(r),
            (Key::BigInt(l), Key::BigInt(r)) => l.type_eq(r),
            (Key::Private(l), Key::Private(r)) => l.type_eq(r),

            (Key::Private(..), _) | (_, Key::Private(..)) => false,

            (Key::Computed(ComputedKey { ty: a, .. }), b) | (b, Key::Computed(ComputedKey { ty: a, .. })) => match b {
                Key::Computed(b) => a.type_eq(&b.ty),
                Key::Normal { sym, .. } => match &**a {
                    Type::Lit(LitType {
                        lit: RTsLit::Str(RStr { value: v, .. }),
                        ..
                    }) => v == sym,
                    Type::Lit(LitType {
                        lit: RTsLit::Number(RNumber { value: v, .. }),
                        ..
                    }) => Ok(*v) == sym.parse::<f64>() && *v == sym.parse::<f64>().unwrap(),
                    _ => false,
                },
                Key::Num(b) => match &**a {
                    Type::Lit(LitType {
                        lit: RTsLit::Str(RStr { value: v, .. }),
                        ..
                    }) => Ok(b.value) == v.parse::<f64>() && *v == b.value.to_string(),
                    Type::Lit(LitType {
                        lit: RTsLit::Number(RNumber { value: v, .. }),
                        ..
                    }) => *v == b.value,
                    _ => false,
                },
                Key::BigInt(b) => match &**a {
                    Type::Lit(LitType {
                        lit: RTsLit::BigInt(RBigInt { value: v, .. }),
                        ..
                    }) => *v == b.value,
                    _ => false,
                },
                Key::Private(..) => false,
            },

            (Key::Num(RNumber { value: n, .. }), Key::Normal { sym: s, .. })
            | (Key::Normal { sym: s, .. }, Key::Num(RNumber { value: n, .. })) => match s.parse::<f64>() {
                Ok(v) => v == *n && *n.to_string() == **s,
                _ => false,
            },

            _ => false,
        }
    }
}

impl Key {
    pub fn normalize(&self) -> Cow<Key> {
        match self {
            Key::Num(v) => Cow::Owned(Key::Normal {
                span: v.span,
                sym: v.value.to_string().into(),
            }),
            _ => Cow::Borrowed(self),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, EqIgnoreSpan, TypeEq, Visit, Spanned, Serialize, Deserialize)]
pub struct PrivateName {
    pub span: Span,
    pub id: Id,
}

impl From<RPrivateName> for PrivateName {
    fn from(n: RPrivateName) -> Self {
        Self {
            span: n.span,
            id: n.id.into(),
        }
    }
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(Key, [u8; 40]);

impl Key {
    /// Returns `true` if this key is a number or a number-like string.
    pub fn is_num_like(&self) -> bool {
        match self {
            Key::Num(..) => true,
            Key::Normal { sym, .. } => sym.parse::<f64>().is_ok(),
            _ => false,
        }
    }

    pub fn ty(&self) -> Cow<Type> {
        match self {
            Key::Computed(prop) => Cow::Borrowed(&*prop.ty),
            Key::Normal { span, sym } => Cow::Owned(Type::Lit(LitType {
                span: *span,
                lit: RTsLit::Str(RStr {
                    span: *span,
                    value: sym.clone(),
                    raw: None,
                }),
                metadata: Default::default(),
                tracker: Default::default(),
            })),
            Key::Num(n) => Cow::Owned(Type::Lit(LitType {
                span: n.span,
                lit: RTsLit::Number(n.clone()),
                metadata: Default::default(),
                tracker: Default::default(),
            })),
            Key::BigInt(n) => Cow::Owned(Type::Lit(LitType {
                span: n.span,
                lit: RTsLit::BigInt(n.clone()),
                metadata: Default::default(),
                tracker: Default::default(),
            })),
            Key::Private(..) => unimplemented!("access to type elements using private name"),
        }
    }
}

impl PartialEq<JsWord> for Key {
    fn eq(&self, other: &JsWord) -> bool {
        match self {
            Key::Normal { sym, .. } => *sym == *other,
            _ => false,
        }
    }
}

impl PartialEq<&JsWord> for Key {
    fn eq(&self, other: &&JsWord) -> bool {
        *self == **other
    }
}

impl PartialEq<str> for Key {
    fn eq(&self, other: &str) -> bool {
        match self {
            Key::Normal { sym, .. } => *sym == *other,
            _ => false,
        }
    }
}

#[derive(Clone, PartialEq, EqIgnoreSpan, Visit, Spanned, Serialize, Deserialize)]
pub struct ComputedKey {
    pub span: Span,
    pub expr: Box<RExpr>,
    pub ty: Box<Type>,
}

impl Debug for ComputedKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{:?}]", self.ty)
    }
}

impl TypeEq for ComputedKey {
    fn type_eq(&self, other: &Self) -> bool {
        self.ty.type_eq(&other.ty)
    }
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(ComputedKey, [u8; 32]);

/// Special type to denote instance of various types.
///
/// This is normalized on any operations. `Analyzer#normalize()` will normalize
/// this variant.
///
/// Used to handle code like
///
///
/// ```ts
/// class Derived {
///     static create() {
///         return new this();
///     }
/// }
/// ```
#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct Instance {
    pub span: Span,
    pub ty: Box<Type>,
    pub metadata: InstanceMetadata,

    pub tracker: Tracker<"Instance">,
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(Instance, [u8; 32]);

#[derive(Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct LitType {
    pub span: Span,

    pub lit: RTsLit,
    pub metadata: LitTypeMetadata,

    pub tracker: Tracker<"LitType">,
}

impl Debug for LitType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.lit {
            RTsLit::Str(s) => write!(f, "'{}'", s.value),
            RTsLit::Number(n) => write!(f, "{}", n.value),
            RTsLit::BigInt(n) => write!(f, "{}n", n.value),
            RTsLit::Bool(b) => write!(f, "{}", b.value),
            RTsLit::Tpl(..) => write!(f, "`<tpl>`"),
        }
    }
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(LitType, [u8; 96]);

#[derive(Clone, PartialEq, Eq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct KeywordType {
    pub span: Span,

    #[use_eq_ignore_span]
    pub kind: TsKeywordTypeKind,
    pub metadata: KeywordTypeMetadata,

    pub tracker: Tracker<"KeywordType">,
}

impl Debug for KeywordType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            TsKeywordTypeKind::TsAnyKeyword => write!(f, "any"),
            TsKeywordTypeKind::TsUnknownKeyword => write!(f, "unknown"),
            TsKeywordTypeKind::TsNumberKeyword => write!(f, "number"),
            TsKeywordTypeKind::TsObjectKeyword => write!(f, "object"),
            TsKeywordTypeKind::TsBooleanKeyword => write!(f, "boolean"),
            TsKeywordTypeKind::TsBigIntKeyword => write!(f, "bigint"),
            TsKeywordTypeKind::TsStringKeyword => write!(f, "string"),
            TsKeywordTypeKind::TsSymbolKeyword => write!(f, "symbol"),
            TsKeywordTypeKind::TsVoidKeyword => write!(f, "void"),
            TsKeywordTypeKind::TsUndefinedKeyword => write!(f, "undefined"),
            TsKeywordTypeKind::TsNullKeyword => write!(f, "null"),
            TsKeywordTypeKind::TsNeverKeyword => write!(f, "never"),
            TsKeywordTypeKind::TsIntrinsicKeyword => write!(f, "intrinsic"),
        }
    }
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(KeywordType, [u8; 28]);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct Symbol {
    pub span: Span,
    pub id: SymbolId,
    pub metadata: SymbolMetadata,

    pub tracker: Tracker<"Symbol">,
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(Symbol, [u8; 48]);

/// Type of form `...T` .
///
///
/// Note: Tuple like `[...T]` is identical to `T[]`.
#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct RestType {
    pub span: Span,
    pub ty: Box<Type>,
    pub metadata: RestTypeMetadata,

    pub tracker: Tracker<"RestType">,
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(RestType, [u8; 32]);

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct OptionalType {
    pub span: Span,
    pub ty: Box<Type>,
    pub metadata: OptionalTypeMetadata,

    pub tracker: Tracker<"OptionalType">,
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(OptionalType, [u8; 32]);

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct IndexedAccessType {
    pub span: Span,
    pub readonly: bool,
    pub obj_type: Box<Type>,
    pub index_type: Box<Type>,
    pub metadata: IndexedAccessTypeMetadata,

    pub tracker: Tracker<"IndexedAccessType">,
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(IndexedAccessType, [u8; 48]);

#[derive(Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize, Debug)]
pub struct Ref {
    pub span: Span,
    #[use_eq_ignore_span]
    pub type_name: RTsEntityName,
    pub type_args: Option<Box<TypeParamInstantiation>>,
    pub metadata: RefMetadata,

    pub tracker: Tracker<"Ref">,
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(Ref, [u8; 72]);

fn write_entity_name(f: &mut Formatter<'_>, name: &RTsEntityName) -> Result<(), fmt::Error> {
    match name {
        RTsEntityName::Ident(i) => write!(f, "{}", i.sym),
        RTsEntityName::TsQualifiedName(q) => {
            write_entity_name(f, &q.left)?;
            write!(f, ".{}", q.right.sym)
        }
    }
}

// impl Debug for Ref {
//     fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
//         write_entity_name(f, &self.type_name)?;

//         if let Some(type_args) = &self.type_args {
//             write!(f, "{:?}", type_args)?
//         }

//         Ok(())
//     }
// }

#[derive(Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct InferType {
    pub span: Span,
    pub type_param: TypeParam,
    pub metadata: InferTypeMetadata,

    pub tracker: Tracker<"InferType">,
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(InferType, [u8; 80]);

impl Debug for InferType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "infer {}", self.type_param.name)
    }
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct QueryType {
    pub span: Span,
    pub expr: Box<QueryExpr>,
    pub metadata: QueryTypeMetadata,

    pub tracker: Tracker<"QueryType">,
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(QueryType, [u8; 32]);

#[derive(Debug, Clone, PartialEq, Spanned, FromVariant, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub enum QueryExpr {
    TsEntityName(#[use_eq_ignore_span] RTsEntityName),
    Import(ImportType),
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct ImportType {
    pub span: Span,
    pub arg: RStr,
    #[use_eq_ignore_span]
    pub qualifier: Option<RTsEntityName>,
    pub type_params: Option<Box<TypeParamInstantiation>>,
    pub metadata: ImportTypeMetadata,

    pub tracker: Tracker<"ImportType">,
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(ImportType, [u8; 96]);

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct Namespace {
    pub span: Span,
    pub name: Id,
    pub exports: Box<ModuleTypeData>,
    pub metadata: NamespaceTypeMetadata,

    pub tracker: Tracker<"Namespace">,
}

#[derive(Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct Module {
    pub span: Span,
    #[use_eq_ignore_span]
    pub name: RTsModuleName,
    pub exports: Box<ModuleTypeData>,
    pub metadata: ModuleTypeMetadata,

    pub tracker: Tracker<"Module">,
}

impl Debug for Module {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        writeln!(f, "module {{")?;

        writeln!(f, "  types:")?;

        for k in self.exports.types.keys() {
            writeln!(f, "    {}", k)?;
        }

        writeln!(f, "  vars:")?;

        for k in self.exports.vars.keys() {
            writeln!(f, "    {}", k)?;
        }

        write!(f, "}}")?;

        Ok(())
    }
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(Module, [u8; 72]);

/// Enum **definition**.
#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct Enum {
    pub span: Span,
    pub declare: bool,
    pub is_const: bool,
    pub id: Id,
    pub members: Vec<EnumMember>,
    pub has_num: bool,
    pub has_str: bool,

    pub metadata: EnumMetadata,

    pub tracker: Tracker<"Enum">,
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(Enum, [u8; 72]);

impl Take for Enum {
    fn dummy() -> Self {
        Self {
            span: DUMMY_SP,
            declare: false,
            is_const: false,
            id: Id::new(js_word!(""), Default::default()),
            members: vec![],
            has_num: false,
            has_str: false,
            metadata: Default::default(),
            tracker: Default::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct EnumMember {
    pub span: Span,
    #[use_eq_ignore_span]
    pub id: RTsEnumMemberId,
    pub val: Box<Type>,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct Class {
    pub span: Span,
    pub def: ArcCow<ClassDef>,
    pub metadata: ClassMetadata,

    pub tracker: Tracker<"Class">,
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(Class, [u8; 40]);

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct ClassDef {
    pub span: Span,
    pub is_abstract: bool,
    pub name: Option<Id>,
    pub super_class: Option<Box<Type>>,
    pub body: Vec<ClassMember>,
    pub type_params: Option<Box<TypeParamDecl>>,
    pub implements: Box<Vec<TsExpr>>,
    pub metadata: ClassDefMetadata,

    pub tracker: Tracker<"ClassDef">,
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(ClassDef, [u8; 96]);

impl Take for ClassDef {
    fn dummy() -> Self {
        Self {
            span: DUMMY_SP,
            is_abstract: false,
            name: None,
            super_class: None,
            body: vec![],
            type_params: None,
            implements: Default::default(),
            metadata: ClassDefMetadata::default(),
            tracker: Default::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Spanned, FromVariant, EqIgnoreSpan, TypeEq, Visit, Is, Serialize, Deserialize)]
pub enum ClassMember {
    Constructor(ConstructorSignature),
    Method(Method),
    Property(ClassProperty),
    IndexSignature(IndexSignature),
}

impl ClassMember {
    pub fn key(&self) -> Option<Cow<Key>> {
        match self {
            ClassMember::Constructor(c) => Some(Cow::Owned(Key::Normal {
                span: c.span,
                sym: js_word!("constructor"),
            })),
            ClassMember::Method(m) => Some(Cow::Borrowed(&m.key)),
            ClassMember::Property(p) => Some(Cow::Borrowed(&p.key)),
            ClassMember::IndexSignature(_) => None,
        }
    }
}

/// A class method.
#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct Method {
    pub span: Span,
    #[use_eq]
    pub accessibility: Option<Accessibility>,
    pub key: Key,
    pub is_static: bool,
    pub is_abstract: bool,
    pub is_optional: bool,
    pub type_params: Option<TypeParamDecl>,
    pub params: Vec<FnParam>,
    pub ret_ty: Box<Type>,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct ClassProperty {
    pub span: Span,
    pub key: Key,
    pub value: Option<Box<Type>>,
    pub is_static: bool,
    #[use_eq]
    pub accessibility: Option<Accessibility>,
    pub is_abstract: bool,
    pub is_optional: bool,
    pub readonly: bool,
    pub definite: bool,

    pub accessor: Accessor,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct Mapped {
    pub span: Span,
    #[use_eq]
    pub readonly: Option<TruePlusMinus>,
    #[use_eq]
    pub optional: Option<TruePlusMinus>,
    pub name_type: Option<Box<Type>>,
    pub type_param: TypeParam,
    pub ty: Option<Box<Type>>,
    pub metadata: MappedMetadata,

    pub tracker: Tracker<"Mapped">,
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(Mapped, [u8; 104]);

#[derive(Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct Conditional {
    pub span: Span,
    pub check_type: Box<Type>,
    pub extends_type: Box<Type>,
    pub true_type: Box<Type>,
    pub false_type: Box<Type>,
    pub metadata: ConditionalMetadata,

    pub tracker: Tracker<"Conditional">,
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(Conditional, [u8; 56]);

impl Debug for Conditional {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?} extends {:?} ? {:?} : {:?}",
            self.check_type, self.extends_type, self.true_type, self.false_type
        )
    }
}

/// `keyof T`
#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct Index {
    pub span: Span,
    pub ty: Box<Type>,
    pub metadata: OperatorMetadata,

    pub tracker: Tracker<"Index">,
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(Index, [u8; 32]);

/// `readonly T`
#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct Readonly {
    pub span: Span,
    pub ty: Box<Type>,
    pub metadata: OperatorMetadata,

    pub tracker: Tracker<"Index">,
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(Readonly, [u8; 32]);

/// Currently only used for `unique symbol`.
#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, Visit, Serialize, Deserialize)]
pub struct Unique {
    pub span: Span,
    pub ty: Box<Type>,
    pub metadata: OperatorMetadata,

    pub tracker: Tracker<"Operator">,
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(Unique, [u8; 32]);

impl TypeEq for Unique {
    #[inline]
    fn type_eq(&self, _: &Self) -> bool {
        false
    }
}

/// This type has a length of n to infinite.
///
/// If the last element is [RestType], this type can have a length of n to
/// infinite.
#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct Tuple {
    pub span: Span,
    pub elems: Vec<TupleElement>,
    pub metadata: TupleMetadata,

    pub tracker: Tracker<"Tuple">,
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(Tuple, [u8; 56]);

#[derive(Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct TupleElement {
    pub span: Span,
    #[not_type]
    pub label: Option<RPat>,
    pub ty: Box<Type>,

    pub tracker: Tracker<"TupleElement">,
}

impl Debug for TupleElement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}: {:?}", self.label, self.ty)
    }
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct Alias {
    pub span: Span,
    pub type_params: Option<Box<TypeParamDecl>>,
    pub ty: Box<Type>,
    pub metadata: AliasMetadata,

    pub tracker: Tracker<"Alias">,
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(Alias, [u8; 40]);

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct Interface {
    pub span: Span,
    pub name: Id,
    pub type_params: Option<Box<TypeParamDecl>>,
    pub extends: Vec<TsExpr>,
    pub body: Vec<TypeElement>,
    pub metadata: InterfaceMetadata,

    pub tracker: Tracker<"Interface">,
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(Interface, [u8; 96]);

#[derive(Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct TypeLit {
    pub span: Span,
    pub members: Vec<TypeElement>,
    pub metadata: TypeLitMetadata,

    pub tracker: Tracker<"TypeLit">,
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(TypeLit, [u8; 56]);

impl Debug for TypeLit {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "{{")?;
        for (i, member) in self.members.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            writeln!(f, "    {:?}", member)?;
        }

        write!(f, "}}")
    }
}

impl TypeLit {
    pub fn is_empty(&self) -> bool {
        self.members.is_empty()
    }
}

#[derive(Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct TypeParamDecl {
    pub span: Span,
    pub params: Vec<TypeParam>,

    pub tracker: Tracker<"TypeParamDecl">,
}

impl Debug for TypeParamDecl {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "<")?;
        for (i, param) in self.params.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{:?}", param)?;
        }
        write!(f, ">")
    }
}

/// Typescript expression with type arguments
#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct TsExpr {
    pub span: Span,
    #[use_eq_ignore_span]
    pub expr: Box<RExpr>,
    pub type_args: Option<Box<TypeParamInstantiation>>,

    pub tracker: Tracker<"TsExpr">,
}

#[derive(Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct TypeParamInstantiation {
    pub span: Span,

    /// TODO(kdy1): Rename to `args`.
    pub params: Vec<Type>,
}

impl Debug for TypeParamInstantiation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "<")?;
        for (i, param) in self.params.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{:?}", param)?;
        }
        write!(f, ">")
    }
}

#[derive(Clone, PartialEq, Spanned, FromVariant, EqIgnoreSpan, TypeEq, Visit, Is, Serialize, Deserialize)]
pub enum TypeElement {
    Call(CallSignature),
    Constructor(ConstructorSignature),
    Property(PropertySignature),
    Method(MethodSignature),
    Index(IndexSignature),
}

impl Debug for TypeElement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeElement::Call(c) => write!(f, "{:?}", c),
            TypeElement::Constructor(c) => write!(f, "{:?}", c),
            TypeElement::Property(p) => write!(f, "{:?}", p),
            TypeElement::Method(m) => write!(f, "{:?}", m),
            TypeElement::Index(i) => write!(f, "{:?}", i),
        }
    }
}

impl Take for TypeElement {
    fn dummy() -> Self {
        Self::Index(Take::dummy())
    }
}

impl TypeElement {
    /// Returns [Some] iff `self` is an element with a normal key.
    pub fn non_computed_key(&self) -> Option<&JsWord> {
        let key = self.key()?;
        match key {
            Key::Normal { sym, .. } => Some(sym),
            _ => None,
        }
    }

    pub fn key(&self) -> Option<&Key> {
        match self {
            TypeElement::Call(..) => None,
            TypeElement::Constructor(..) => None,
            TypeElement::Property(p) => Some(&p.key),
            TypeElement::Method(m) => Some(&m.key),
            TypeElement::Index(_) => None,
        }
    }

    pub fn get_type(&self) -> Option<&Type> {
        match self {
            TypeElement::Call(CallSignature { ret_ty, .. }) => ret_ty,
            TypeElement::Constructor(ConstructorSignature { ret_ty, .. }) => ret_ty,
            TypeElement::Property(PropertySignature { type_ann, .. }) => type_ann,
            TypeElement::Method(MethodSignature { ret_ty, .. }) => ret_ty,
            TypeElement::Index(IndexSignature { type_ann, .. }) => type_ann,
        }
        .as_deref()
    }
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct CallSignature {
    pub span: Span,
    pub params: Vec<FnParam>,
    pub type_params: Option<TypeParamDecl>,
    pub ret_ty: Option<Box<Type>>,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct ConstructorSignature {
    pub span: Span,
    /// Only for synthesized type elements.
    #[use_eq]
    pub accessibility: Option<Accessibility>,
    pub params: Vec<FnParam>,
    pub ret_ty: Option<Box<Type>>,
    pub type_params: Option<TypeParamDecl>,
}

#[derive(Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct PropertySignature {
    pub span: Span,
    /// Only for synthesized type elements.
    #[use_eq]
    pub accessibility: Option<Accessibility>,
    pub readonly: bool,
    pub key: Key,
    pub optional: bool,
    pub params: Vec<FnParam>,
    pub type_ann: Option<Box<Type>>,
    pub type_params: Option<TypeParamDecl>,
    pub metadata: TypeElMetadata,

    pub accessor: Accessor,
}

impl Debug for PropertySignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", &self.key)?;

        if let Some(type_ann) = &self.type_ann {
            write!(f, ": {:?}", type_ann)?;
        }

        Ok(())
    }
}

#[derive(Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct MethodSignature {
    pub span: Span,
    /// Only for synthesized type elements.
    #[use_eq]
    pub accessibility: Option<Accessibility>,
    pub readonly: bool,
    pub key: Key,
    pub optional: bool,
    pub params: Vec<FnParam>,
    pub ret_ty: Option<Box<Type>>,
    pub type_params: Option<TypeParamDecl>,
    pub metadata: TypeElMetadata,
}

impl Debug for MethodSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", &self.key)?;
        if let Some(type_params) = &self.type_params {
            write!(f, "{:?}", type_params)?;
        }
        write!(f, "(")?;
        for (i, param) in self.params.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{:?}", param)?;
        }
        write!(f, ")")?;
        if let Some(ret_ty) = &self.ret_ty {
            write!(f, ": {:?}", ret_ty)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct IndexSignature {
    pub span: Span,

    pub params: Vec<FnParam>,
    pub type_ann: Option<Box<Type>>,

    pub readonly: bool,

    pub is_static: bool,
}

impl Take for IndexSignature {
    fn dummy() -> Self {
        Self {
            params: Take::dummy(),
            type_ann: Take::dummy(),
            readonly: false,
            span: Take::dummy(),
            is_static: false,
        }
    }
}

#[derive(Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct Array {
    pub span: Span,
    pub elem_type: Box<Type>,
    pub metadata: ArrayMetadata,

    pub tracker: Tracker<"Array">,
}

impl Debug for Array {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}[]", self.elem_type)
    }
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(Array, [u8; 32]);

/// a | b
#[derive(Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct Union {
    pub span: Span,
    pub types: Vec<Type>,
    pub metadata: UnionMetadata,

    pub tracker: Tracker<"Union">,
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(Union, [u8; 48]);

impl Debug for Union {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;

        for (i, ty) in self.types.iter().enumerate() {
            if i != 0 {
                write!(f, " | ")?;
            }
            Debug::fmt(&ty, f)?;
        }

        write!(f, ")")?;

        Ok(())
    }
}

impl Union {
    pub fn assert_valid(&self) {
        if !cfg!(debug_assertions) {
            return;
        }

        self.visit_with(&mut AssertValid);
    }
}

#[derive(Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct FnParam {
    pub span: Span,
    pub required: bool,
    #[not_type]
    pub pat: RPat,
    pub ty: Box<Type>,
}

impl Debug for FnParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;

        if !self.required {
            write!(f, "?")?;
        }

        write!(f, "{:?}", self.ty)?;

        write!(f, ")")
    }
}

/// a & b
#[derive(Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct Intersection {
    pub span: Span,
    pub types: Vec<Type>,
    pub metadata: IntersectionMetadata,

    pub tracker: Tracker<"Intersection">,
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(Intersection, [u8; 48]);

impl Debug for Intersection {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;

        for (i, ty) in self.types.iter().enumerate() {
            if i != 0 {
                write!(f, " & ")?;
            }
            Debug::fmt(&ty, f)?;
        }

        write!(f, ")")?;

        Ok(())
    }
}

impl Intersection {
    pub fn assert_valid(&self) {
        if !cfg!(debug_assertions) {
            return;
        }

        self.visit_with(&mut AssertValid);
    }

    pub fn is_trivial_never(iter: &[Type]) -> bool {
        let mut tys = vec![];

        for ty in iter {
            if let Type::Intersection(Intersection { types, .. }) = ty {
                tys.extend(types);
            } else {
                tys.push(ty);
            }
        }
        tys.dedup_type();

        if tys.iter().any(|ty| ty.is_never()) {
            return true;
        }

        let is_symbol = tys.iter().any(|ty| ty.is_symbol());
        let is_str = tys.iter().any(|ty| ty.is_str());
        let is_num = tys.iter().any(|ty| ty.is_num());
        let is_bool = tys.iter().any(|ty| ty.is_bool());
        let is_null = tys.iter().any(|ty| ty.is_null());
        let is_undefined = tys.iter().any(|ty| ty.is_undefined());
        let is_void = tys.iter().any(|ty| ty.is_kwd(TsKeywordTypeKind::TsVoidKeyword));
        let is_object = tys.iter().any(|ty| ty.is_kwd(TsKeywordTypeKind::TsObjectKeyword));
        let is_function = tys.iter().any(|ty| ty.is_fn_type());
        let is_type_lit = tys.iter().any(|ty| ty.is_type_lit());

        if (is_null || is_undefined) && is_type_lit {
            return true;
        }

        let sum = u32::from(is_symbol)
            + u32::from(is_str)
            + u32::from(is_num)
            + u32::from(is_bool)
            + u32::from(is_null)
            + u32::from(is_undefined)
            + u32::from(is_void)
            + u32::from(is_object)
            + u32::from(is_function);

        if sum > 1 {
            if sum == 2 && is_undefined && is_void {
                return false;
            }
            return true;
        }

        false
    }
}

/// A type parameter
#[derive(Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize, Debug)]
pub struct TypeParam {
    pub span: Span,
    pub name: Id,
    pub constraint: Option<Box<Type>>,
    pub default: Option<Box<Type>>,
    pub metadata: TypeParamMetadata,

    pub tracker: Tracker<"TypeParam">,
}

// impl Debug for TypeParam {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         write!(f, "{}", self.name)?;

//         Ok(())
//     }
// }

/// FooEnum.A
#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct EnumVariant {
    pub span: Span,
    pub def: ArcCow<Enum>,
    /// [None] if for the general instance type of an enum.
    pub name: Option<JsWord>,
    pub metadata: EnumVariantMetadata,

    pub tracker: Tracker<"EnumVariant">,
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(EnumVariant, [u8; 48]);

#[derive(Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct Function {
    pub span: Span,
    pub type_params: Option<TypeParamDecl>,
    pub params: Vec<FnParam>,
    pub ret_ty: Box<Type>,
    pub metadata: FunctionMetadata,

    pub tracker: Tracker<"Function">,
}

impl Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;

        if let Some(type_params) = &self.type_params {
            write!(f, "{:?}", type_params)?;
        }

        write!(f, "(")?;

        for (i, param) in self.params.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{:?}", param)?;
        }
        write!(f, ") => {:?}", self.ret_ty)?;

        write!(f, ")")?;

        Ok(())
    }
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(Function, [u8; 96]);

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct Constructor {
    pub span: Span,
    pub type_params: Option<TypeParamDecl>,
    pub params: Vec<FnParam>,
    /// The return type.
    pub type_ann: Box<Type>,
    pub is_abstract: bool,
    pub metadata: ConstructorMetadata,

    pub tracker: Tracker<"Constructor">,
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(Constructor, [u8; 104]);

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct Predicate {
    pub span: Span,
    #[use_eq_ignore_span]
    pub param_name: RTsThisTypeOrIdent,
    pub asserts: bool,
    pub ty: Option<Box<Type>>,
    pub metadata: PredicateMetadata,

    pub tracker: Tracker<"Predicate">,
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(Predicate, [u8; 72]);

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct TypeOrSpread {
    pub span: Span,
    pub spread: Option<Span>,
    pub ty: Box<Type>,
}

pub trait TypeIterExt {}

struct AssertCloneCheap;

impl Visit<Type> for AssertCloneCheap {
    fn visit(&mut self, ty: &Type) {
        if cfg![debug_assertions] && !ty.is_clone_cheap() {
            unreachable!("{:?} is not cheap to clone", ty);
        }
    }
}

impl Type {
    #[cfg_attr(not(debug_assertions), inline(always))]
    pub fn assert_clone_cheap(&self) {
        if !cfg!(debug_assertions) {
            return;
        }

        // let _ctx = panic_ctx!(format!("assert_clone_cheap: {:?}", self));

        self.visit_with(&mut AssertCloneCheap);
    }

    pub fn new_intersection<I>(span: Span, iter: I) -> Self
    where
        I: IntoIterator<Item = Type>,
    {
        let mut tys = vec![];

        for ty in iter {
            if ty.is_intersection() {
                tys.extend(ty.expect_intersection().types);
            } else {
                tys.push(ty);
            }
        }
        tys.dedup_type();

        if Intersection::is_trivial_never(&tys) {
            return Type::never(span, Default::default());
        }

        if tys.len() > 1 {
            // In an intersection everything absorbs unknown
            tys.retain(|ty| !ty.is_unknown());
        }

        if tys.len() > 1
            && !(tys.len() == 2 && tys.iter().any(|ty| ty.is_type_param() || ty.is_conditional()))
            && tys.iter().any(|ty| ty.is_type_lit())
        {
            // reduce empty type lit
            tys.retain(|ty| {
                if let Type::TypeLit(ty) = ty.normalize() {
                    !ty.is_empty()
                } else {
                    true
                }
            });
        }

        match tys.len() {
            0 => Type::never(span, Default::default()),
            1 => tys.into_iter().next().unwrap(),
            _ => Type::Intersection(Intersection {
                span,
                types: tys,
                metadata: Default::default(),
                tracker: Default::default(),
            }),
        }
    }

    pub fn new_union_without_dedup(span: Span, types: Vec<Type>) -> Self {
        let ty = match types.len() {
            0 => Type::never(span, Default::default()),
            1 => types.into_iter().next().unwrap(),
            _ => {
                if types.iter().any(|t| t.is_union_type()) {
                    let mut elements = vec![];

                    for ty in types {
                        if ty.is_unknown() {
                            // In a union an unknown absorbs everything
                            return Type::Keyword(KeywordType {
                                span,
                                kind: TsKeywordTypeKind::TsUnknownKeyword,
                                metadata: Default::default(),
                                tracker: Default::default(),
                            });
                        }

                        if ty.is_union_type() {
                            let types = ty.expect_union_type().types;
                            for new in types {
                                elements.push(new)
                            }
                        } else {
                            elements.push(ty)
                        }
                    }
                    return Type::Union(Union {
                        span,
                        types: elements,
                        metadata: Default::default(),
                        tracker: Default::default(),
                    });
                }

                Type::Union(Union {
                    span,
                    types,
                    metadata: Default::default(),
                    tracker: Default::default(),
                })
            }
        };
        ty.assert_valid();
        ty
    }

    pub fn new_union<I: IntoIterator<Item = Self> + Debug>(span: Span, iter: I) -> Self {
        let mut elements = vec![];

        for ty in iter {
            if ty.is_unknown() {
                // In a union an unknown absorbs everything
                return Type::Keyword(KeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsUnknownKeyword,
                    metadata: Default::default(),
                    tracker: Default::default(),
                });
            }

            if ty.is_union_type() {
                let types = ty.expect_union_type().types;
                for new in types {
                    if elements.iter().any(|prev: &Type| prev.type_eq(&new)) {
                        continue;
                    }
                    elements.push(new)
                }
            } else {
                if elements.iter().any(|prev: &Type| prev.type_eq(&ty)) {
                    continue;
                }
                elements.push(ty)
            }
        }
        // Drop `never`s.
        elements.retain(|ty| !ty.is_never());

        Self::new_union_without_dedup(span, elements)
    }

    pub fn union_with_undefined(self, span: Span) -> Type {
        match self.normalize() {
            Type::Union(u) => {
                if u.types.iter().any(|ty| ty.is_undefined()) {
                    return self;
                }

                let mut u = self.expect_union_type();
                u.types.push(Type::undefined(span, Default::default()));
                Type::Union(u)
            }

            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsAnyKeyword | TsKeywordTypeKind::TsUnknownKeyword,
                ..
            }) => self,

            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsNeverKeyword | TsKeywordTypeKind::TsUndefinedKeyword,
                ..
            }) => Type::undefined(span, Default::default()),

            _ => Self::new_union_without_dedup(span, vec![self, Type::undefined(span, Default::default())]),
        }
    }

    /// If `self` is [Type::Lit], convert it to [Type::Keyword].
    pub fn force_generalize_top_level_literals(self) -> Self {
        match self {
            Type::Lit(lit) => Type::Keyword(KeywordType {
                span: lit.span,
                kind: match lit.lit {
                    RTsLit::BigInt(_) => TsKeywordTypeKind::TsBigIntKeyword,
                    RTsLit::Number(_) => TsKeywordTypeKind::TsNumberKeyword,
                    RTsLit::Str(_) => TsKeywordTypeKind::TsStringKeyword,
                    RTsLit::Bool(_) => TsKeywordTypeKind::TsBooleanKeyword,
                    RTsLit::Tpl(_) => {
                        unreachable!()
                    }
                },
                metadata: KeywordTypeMetadata {
                    common: lit.metadata.common,
                    ..Default::default()
                },
                tracker: Default::default(),
            }),
            _ => self,
        }
    }

    pub fn is_void(&self) -> bool {
        self.is_kwd(TsKeywordTypeKind::TsVoidKeyword)
    }

    pub fn contains_void(&self) -> bool {
        match self.normalize() {
            Type::Instance(ty) => ty.ty.contains_void(),

            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsVoidKeyword,
                ..
            }) => true,

            Type::Union(ref t) => t.types.iter().any(|t| t.contains_void()),

            _ => false,
        }
    }

    pub fn is_any(&self) -> bool {
        match self.normalize_instance() {
            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsAnyKeyword,
                ..
            }) => true,

            Type::Union(t) => t.types.iter().any(|t| t.is_any()),

            _ => false,
        }
    }

    pub fn is_unknown(&self) -> bool {
        match self.normalize_instance() {
            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsUnknownKeyword,
                ..
            }) => true,

            Type::Union(t) => t.types.iter().any(|t| t.is_unknown()),

            _ => false,
        }
    }

    pub fn contains_undefined(&self) -> bool {
        match *self.normalize() {
            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsUndefinedKeyword,
                ..
            }) => true,

            Type::Union(ref t) => t.types.iter().any(|t| t.contains_undefined()),

            _ => false,
        }
    }

    /// Returns [Some] if `self` is an array or an readonly array.
    pub fn as_array_without_readonly(&self) -> Option<&Array> {
        match self.normalize_instance() {
            Type::Array(t) => Some(t),
            Type::Readonly(ty) => ty.ty.as_array_without_readonly(),
            _ => None,
        }
    }
}

impl Type {
    /// TODO
    pub fn is_clone_cheap(&self) -> bool {
        match self {
            Type::Arc(..) | Type::Keyword(..) | Type::Lit(..) | Type::This(..) | Type::StaticThis(..) | Type::Symbol(..) => true,

            // TODO(kdy1): Make this false.
            Type::Param(TypeParam { constraint, default, .. }) => {
                constraint.as_ref().map(|ty| ty.is_clone_cheap()).unwrap_or(true)
                    && default.as_ref().map(|ty| ty.is_clone_cheap()).unwrap_or(true)
            }

            _ => false,
        }
    }

    pub fn is_null(&self) -> bool {
        self.is_kwd(TsKeywordTypeKind::TsNullKeyword)
    }

    pub fn is_undefined(&self) -> bool {
        self.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword)
    }

    pub fn is_null_or_undefined(&self) -> bool {
        self.is_null() || self.is_undefined()
    }

    pub fn is_kwd(&self, k: TsKeywordTypeKind) -> bool {
        match self.normalize() {
            Type::Instance(ty) => ty.ty.is_kwd(k),
            Type::Keyword(KeywordType { kind, .. }) if *kind == k => true,
            _ => false,
        }
    }

    pub fn is_unique_symbol(&self) -> bool {
        match self.normalize_instance() {
            Type::Unique(u) => u.ty.is_kwd(TsKeywordTypeKind::TsSymbolKeyword),
            _ => false,
        }
    }

    pub fn is_symbol_like(&self) -> bool {
        self.is_symbol() || self.is_unique_symbol() || self.is_kwd(TsKeywordTypeKind::TsSymbolKeyword)
    }

    pub fn is_never(&self) -> bool {
        self.is_kwd(TsKeywordTypeKind::TsNeverKeyword)
    }

    pub fn never(span: Span, metadata: KeywordTypeMetadata) -> Self {
        Type::Keyword(KeywordType {
            span,
            kind: TsKeywordTypeKind::TsNeverKeyword,
            metadata,
            tracker: Default::default(),
        })
    }

    pub fn undefined(span: Span, metadata: KeywordTypeMetadata) -> Self {
        Type::Keyword(KeywordType {
            span,
            kind: TsKeywordTypeKind::TsUndefinedKeyword,
            metadata,
            tracker: Default::default(),
        })
    }

    pub fn any(span: Span, metadata: KeywordTypeMetadata) -> Self {
        Type::Keyword(KeywordType {
            span,
            kind: TsKeywordTypeKind::TsAnyKeyword,
            metadata,
            tracker: Default::default(),
        })
    }

    pub fn void(span: Span, metadata: KeywordTypeMetadata) -> Self {
        Type::Keyword(KeywordType {
            span,
            kind: TsKeywordTypeKind::TsVoidKeyword,
            metadata,
            tracker: Default::default(),
        })
    }

    pub fn unknown(span: Span, metadata: KeywordTypeMetadata) -> Self {
        Type::Keyword(KeywordType {
            span,
            kind: TsKeywordTypeKind::TsUnknownKeyword,
            metadata,
            tracker: Default::default(),
        })
    }

    pub fn is_str_like(&self) -> bool {
        matches!(
            self.normalize_instance(),
            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsStringKeyword,
                ..
            }) | Type::Lit(LitType { lit: RTsLit::Str(..), .. })
                | Type::Tpl(..)
                | Type::StringMapping(..)
        )
    }

    pub fn is_num_like(&self) -> bool {
        matches!(
            self.normalize_instance(),
            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsNumberKeyword,
                ..
            }) | Type::Lit(LitType {
                lit: RTsLit::Number(..),
                ..
            })
        )
    }

    pub fn is_bool_like(&self) -> bool {
        matches!(
            self.normalize_instance(),
            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsBooleanKeyword,
                ..
            }) | Type::Lit(LitType { lit: RTsLit::Bool(..), .. })
        )
    }

    pub fn is_bigint_like(&self) -> bool {
        matches!(
            self.normalize_instance(),
            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsBigIntKeyword,
                ..
            }) | Type::Lit(LitType {
                lit: RTsLit::BigInt(..),
                ..
            })
        )
    }

    pub fn is_structured(&self) -> bool {
        self.is_type_lit() || self.is_union_type() || self.is_intersection()
    }

    pub fn is_substitution(&self) -> bool {
        false
    }

    pub fn is_instantiable_non_primitive(&self) -> bool {
        self.is_type_param() || self.is_conditional() || self.is_substitution()
    }

    pub fn is_instantiable_primitive(&self) -> bool {
        self.is_index() || self.is_tpl() || self.is_string_mapping()
    }

    pub fn is_instantiable(&self) -> bool {
        self.is_instantiable_non_primitive() || self.is_instantiable_primitive()
    }

    pub fn is_structured_or_instantiable(&self) -> bool {
        self.is_structured() || self.is_instantiable()
    }
}

impl Type {
    pub fn metadata(&self) -> CommonTypeMetadata {
        match self.normalize() {
            Type::Instance(ty) => ty.metadata.common,
            Type::StaticThis(ty) => ty.metadata.common,
            Type::This(ty) => ty.metadata.common,
            Type::Lit(ty) => ty.metadata.common,
            Type::Query(ty) => ty.metadata.common,
            Type::Infer(ty) => ty.metadata.common,
            Type::Import(ty) => ty.metadata.common,
            Type::Predicate(ty) => ty.metadata.common,
            Type::IndexedAccessType(ty) => ty.metadata.common,
            Type::Ref(ty) => ty.metadata.common,
            Type::TypeLit(ty) => ty.metadata.common,
            Type::Keyword(ty) => ty.metadata.common,
            Type::Conditional(ty) => ty.metadata.common,
            Type::Tuple(ty) => ty.metadata.common,
            Type::Array(ty) => ty.metadata.common,
            Type::Union(ty) => ty.metadata.common,
            Type::Intersection(ty) => ty.metadata.common,
            Type::Function(ty) => ty.metadata.common,
            Type::Constructor(ty) => ty.metadata.common,
            Type::Index(ty) => ty.metadata.common,
            Type::Readonly(ty) => ty.metadata.common,
            Type::Unique(ty) => ty.metadata.common,
            Type::Param(ty) => ty.metadata.common,
            Type::EnumVariant(ty) => ty.metadata.common,
            Type::Interface(ty) => ty.metadata.common,
            Type::Enum(ty) => ty.metadata.common,
            Type::Mapped(ty) => ty.metadata.common,
            Type::Alias(ty) => ty.metadata.common,
            Type::Namespace(ty) => ty.metadata.common,
            Type::Module(ty) => ty.metadata.common,
            Type::Class(ty) => ty.metadata.common,
            Type::ClassDef(ty) => ty.metadata.common,
            Type::Rest(ty) => ty.metadata.common,
            Type::Optional(ty) => ty.metadata.common,
            Type::Symbol(ty) => ty.metadata.common,
            Type::Tpl(ty) => ty.metadata.common,
            Type::StringMapping(ty) => ty.metadata.common,

            Type::Arc(_) => unreachable!(),
        }
    }

    pub fn metadata_mut(&mut self) -> &mut CommonTypeMetadata {
        match self.normalize_mut() {
            Type::Instance(ty) => &mut ty.metadata.common,
            Type::StaticThis(ty) => &mut ty.metadata.common,
            Type::This(ty) => &mut ty.metadata.common,
            Type::Lit(ty) => &mut ty.metadata.common,
            Type::Query(ty) => &mut ty.metadata.common,
            Type::Infer(ty) => &mut ty.metadata.common,
            Type::Import(ty) => &mut ty.metadata.common,
            Type::Predicate(ty) => &mut ty.metadata.common,
            Type::IndexedAccessType(ty) => &mut ty.metadata.common,
            Type::Ref(ty) => &mut ty.metadata.common,
            Type::TypeLit(ty) => &mut ty.metadata.common,
            Type::Keyword(ty) => &mut ty.metadata.common,
            Type::Conditional(ty) => &mut ty.metadata.common,
            Type::Tuple(ty) => &mut ty.metadata.common,
            Type::Array(ty) => &mut ty.metadata.common,
            Type::Union(ty) => &mut ty.metadata.common,
            Type::Intersection(ty) => &mut ty.metadata.common,
            Type::Function(ty) => &mut ty.metadata.common,
            Type::Constructor(ty) => &mut ty.metadata.common,
            Type::Index(ty) => &mut ty.metadata.common,
            Type::Readonly(ty) => &mut ty.metadata.common,
            Type::Unique(ty) => &mut ty.metadata.common,
            Type::Param(ty) => &mut ty.metadata.common,
            Type::EnumVariant(ty) => &mut ty.metadata.common,
            Type::Interface(ty) => &mut ty.metadata.common,
            Type::Enum(ty) => &mut ty.normalize_mut().metadata.common,
            Type::Mapped(ty) => &mut ty.metadata.common,
            Type::Alias(ty) => &mut ty.metadata.common,
            Type::Namespace(ty) => &mut ty.metadata.common,
            Type::Module(ty) => &mut ty.metadata.common,
            Type::Class(ty) => &mut ty.metadata.common,
            Type::ClassDef(ty) => &mut ty.normalize_mut().metadata.common,
            Type::Rest(ty) => &mut ty.metadata.common,
            Type::Optional(ty) => &mut ty.metadata.common,
            Type::Symbol(ty) => &mut ty.metadata.common,
            Type::Tpl(ty) => &mut ty.metadata.common,
            Type::StringMapping(ty) => &mut ty.metadata.common,

            Type::Arc(_) => unreachable!(),
        }
    }

    /// Respan but preserve SyntaxContext
    pub fn reposition(&mut self, from: Span) {
        let ctxt = self.span().ctxt;
        let span = from.with_ctxt(ctxt);
        self.respan(span)
    }

    pub fn respan(&mut self, span: Span) {
        if self.span() == span {
            return;
        }

        match self.normalize_mut() {
            Type::Index(ty) => ty.span = span,

            Type::Readonly(ty) => ty.span = span,

            Type::Unique(ty) => ty.span = span,

            Type::Mapped(ty) => ty.span = span,

            Type::Conditional(cond) => cond.span = span,

            Type::This(this) => this.span = span,

            Type::Lit(lit) => lit.span = span,

            Type::TypeLit(lit) => lit.span = span,

            Type::Keyword(kwd) => kwd.span = span,

            Type::Array(arr) => arr.span = span,

            Type::Union(u) => u.span = span,

            Type::Intersection(u) => u.span = span,

            Type::Function(f) => f.span = span,

            Type::Constructor(c) => c.span = span,

            Type::Enum(e) => e.normalize_mut().span = span,

            Type::EnumVariant(e) => e.span = span,

            Type::Interface(e) => e.span = span,

            Type::Alias(a) => a.span = span,

            Type::Namespace(n) => n.span = span,

            Type::Module(m) => m.span = span,

            Type::Class(c) => c.span = span,

            Type::ClassDef(c) => c.normalize_mut().span = span,

            Type::Param(p) => p.span = span,

            Type::Tuple(ty) => ty.span = span,

            Type::Ref(ty) => ty.span = span,

            Type::Query(ty) => ty.span = span,

            Type::Infer(ty) => ty.span = span,

            Type::Import(ty) => ty.span = span,

            Type::Predicate(ty) => ty.span = span,

            Type::IndexedAccessType(ty) => ty.span = span,

            Type::Optional(ty) => ty.span = span,

            Type::Rest(ty) => ty.span = span,

            Type::Symbol(ty) => ty.span = span,

            Type::StaticThis(ty) => ty.span = span,

            Type::Instance(ty) => ty.span = span,

            Type::Tpl(ty) => ty.span = span,

            Type::StringMapping(ty) => ty.span = span,

            Type::Arc(..) => {
                unreachable!()
            }
        }
    }
}

/// Visitor which validate types.
///
/// See [Type] for variants which should be kept by [Type]s.
struct AssertValid;

impl Visit<TypeElement> for AssertValid {
    fn visit(&mut self, el: &TypeElement) {
        if !cfg!(debug_assertions) {
            return;
        }
        el.visit_children_with(self);
        debug_assert_eq!(el.span().ctxt, SyntaxContext::empty());
    }
}

impl Visit<EnumMember> for AssertValid {
    fn visit(&mut self, el: &EnumMember) {
        if !cfg!(debug_assertions) {
            return;
        }
        el.visit_children_with(self);
        debug_assert_eq!(el.span().ctxt, SyntaxContext::empty());
    }
}

impl Visit<ClassMember> for AssertValid {
    fn visit(&mut self, el: &ClassMember) {
        if !cfg!(debug_assertions) {
            return;
        }
        el.visit_children_with(self);
        debug_assert_eq!(el.span().ctxt, SyntaxContext::empty());
    }
}

impl Visit<Union> for AssertValid {
    fn visit(&mut self, ty: &Union) {
        if !cfg!(debug_assertions) {
            return;
        }

        ty.visit_children_with(self);

        for (i, t1) in ty.types.iter().enumerate() {
            for (j, t2) in ty.types.iter().enumerate() {
                if i == j {
                    continue;
                }
                if t1.type_eq(t2) {
                    unreachable!("[INVALID_TYPE]: A union type has duplicate elements: ({:?})", t1)
                }
            }
        }

        if ty.types.len() <= 1 {
            unreachable!("[INVALID_TYPE]: A union type should have multiple items. Got {:?}", ty.types);
        }

        for item in ty.types.iter() {
            if item.is_union_type() {
                unreachable!("[INVALID_TYPE]: A union type should not have a union item")
            }
        }
    }
}

impl Visit<Intersection> for AssertValid {
    fn visit(&mut self, ty: &Intersection) {
        if !cfg!(debug_assertions) {
            return;
        }

        ty.visit_children_with(self);

        for (i, t1) in ty.types.iter().enumerate() {
            for (j, t2) in ty.types.iter().enumerate() {
                if i == j {
                    continue;
                }
                if t1.type_eq(t2) {
                    unreachable!("[INVALID_TYPE]: An intersection type has duplicate elements: ({:?})", t1)
                }
            }
        }

        if ty.types.len() <= 1 {
            unreachable!(
                "[INVALID_TYPE]: An intersection type should have multiple items. Got {:?}",
                ty.types
            );
        }

        for item in ty.types.iter() {
            if item.is_intersection() {
                unreachable!("[INVALID_TYPE]: An intersection type should not have an intersection item")
            }
        }
    }
}

impl Type {
    pub fn get_type_param_decl(&self) -> Option<&TypeParamDecl> {
        match self.normalize() {
            Type::Class(ty) => ty.def.type_params.as_deref(),
            Type::ClassDef(ty) => ty.type_params.as_deref(),
            Type::Interface(Interface { ref type_params, .. }) | Type::Alias(Alias { ref type_params, .. }) => type_params.as_deref(),
            _ => None,
        }
    }

    /// Panics if type is invalid. This is debug-build only and it's noop on a
    /// release build.
    ///
    /// # Validity
    ///
    /// For example, `any | any` is invalid because
    /// union should not have duplicate elements.
    pub fn assert_valid(&self) {
        if !cfg!(debug_assertions) {
            return;
        }

        self.visit_with(&mut AssertValid);
    }

    pub fn is_global_this(&self) -> bool {
        match self.normalize() {
            Type::Query(QueryType {
                expr: box QueryExpr::TsEntityName(RTsEntityName::Ident(i)),
                ..
            }) => &*i.sym == "globalThis",
            _ => false,
        }
    }
}

//
//impl Type {
//    pub fn into_static(self) -> Type {
//        match self {
//            Type::Operator(ty) => Type::Operator(ty),
//            Type::Mapped(ty) => Type::Mapped(ty),
//            Type::Conditional(cond) => Type::Conditional(cond),
//            Type::This(this) => Type::This(this),
//            Type::TypeLit(lit) => Type::TypeLit(lit),
//            Type::Lit(lit) => Type::Lit(lit),
//            Type::Keyword(lit) => Type::Keyword(lit),
//            Type::Simple(s) => Type::Simple(s),
//            Type::Array(Array { span, elem_type }) => Type::Array(Array {
//                span,
//                elem_type: box static_type(*elem_type),
//            }),
//
//            Type::Union(Union { span, types }) => Type::Union(Union {
//                span,
//                types: map_types(types, static_type),
//            }),
//            Type::Intersection(Intersection { span, types }) =>
// Type::Intersection(Intersection {                span,
//                types: map_types(types, static_type),
//            }),
//
//            Type::Function(Function {
//                span,
//                type_params,
//                params,
//                ret_ty,
//            }) => Type::Function(Function {
//                span,
//                type_params: type_params.map(|v| v),
//                params,
//                ret_ty: box static_type(*ret_ty),
//            }),
//
//            Type::Constructor(Constructor {
//                span,
//                type_params,
//                params,
//                ret_ty,
//            }) => Type::Constructor(Constructor {
//                span,
//                type_params: type_params.map(|v| v),
//                params,
//                ret_ty: ret_ty.map(|ret_ty| box static_type(*ret_ty)),
//            }),
//
//            Type::Method(m) => Type::Method(m),
//
//            Type::Interface(i) => Type::Interface(i),
//
//            Type::Param(p) => Type::Param(p),
//
//            Type::Enum(e) => Type::Enum(e),
//            Type::EnumVariant(e) => Type::EnumVariant(e),
//            Type::Class(c) => Type::Class(c),
//            Type::ClassInstance(c) => Type::ClassInstance(c),
//            Type::Alias(a) => Type::Alias(a),
//            Type::Namespace(n) => Type::Namespace(n),
//            Type::Module(m) => Type::Module(m),
//
//            Type::Arc(ty) => Type::Arc(ty),
//
//            Type::Static(s) => Type::Static(s),
//
//            Type::Tuple(t) => Type::Tuple(t),
//        }
//    }
//}

impl Type {
    /// Converts this type to foldable type.
    ///
    /// TODO(kdy1): Remove if possible
    pub fn foldable(mut self) -> Type {
        self.normalize_mut();
        self
    }

    /// [Type::Arc] is normalized.
    pub fn normalize<'s, 'c>(&'s self) -> &'c Type
    where
        's: 'c,
    {
        match *self {
            Type::Arc(ref s) => {
                //
                unsafe { transmute::<&'s Type, &'c Type>(&s.ty) }
            }
            _ => unsafe {
                // Shorten lifetimes
                transmute::<&'s Self, &'c Type>(self)
            },
        }
    }

    /// [Type::Arc] and [Type::Instance] are normalized.
    pub fn normalize_instance<'s, 'c>(&'s self) -> &'c Type
    where
        's: 'c,
    {
        let ty = self.normalize();
        match ty {
            Type::Instance(ty) => ty.ty.normalize_instance(),
            _ => ty,
        }
    }

    /// `Type::Static` is normalized.
    #[instrument(skip_all)]
    pub fn normalize_mut(&mut self) -> &mut Type {
        if let Type::Arc(Freezed { ty }) = self {
            let ty = Arc::make_mut(ty);
            *self = replace(ty, Type::any(DUMMY_SP, Default::default()));
        }

        self
    }

    pub fn iter_union(&self) -> impl Debug + Iterator<Item = &Type> {
        UnionIter {
            ty: self.normalize(),
            idx: 0,
        }
    }

    pub fn iter_intersection(&self) -> impl Debug + Iterator<Item = &Type> {
        IntersectionIter {
            ty: self.normalize(),
            idx: 0,
        }
    }
}

#[derive(Debug)]
struct UnionIter<'a> {
    ty: &'a Type,
    idx: usize,
}

impl<'a> Iterator for UnionIter<'a> {
    type Item = &'a Type;

    fn next(&mut self) -> Option<Self::Item> {
        match &self.ty {
            Type::Union(ref u) => {
                let ty = u.types.get(self.idx);
                self.idx += 1;
                ty
            }

            _ if self.idx == 0 => {
                self.idx = 1;
                Some(self.ty)
            }

            _ => None,
        }
    }
}

impl FusedIterator for UnionIter<'_> {}

#[derive(Debug)]
struct IntersectionIter<'a> {
    ty: &'a Type,
    idx: usize,
}

impl<'a> Iterator for IntersectionIter<'a> {
    type Item = &'a Type;

    fn next(&mut self) -> Option<Self::Item> {
        match &self.ty {
            Type::Intersection(ref u) => {
                let ty = u.types.get(self.idx);
                self.idx += 1;
                ty
            }

            _ if self.idx == 0 => {
                self.idx = 1;
                Some(self.ty)
            }

            _ => None,
        }
    }
}

impl FusedIterator for IntersectionIter<'_> {}

impl Type {
    /// Return true if `self` is a [Type::Ref] pointing to `name`.
    pub fn is_builtin_interface(&self, name: &str) -> bool {
        match self.normalize_instance() {
            Type::Ref(ref r) => {
                if let RTsEntityName::Ident(ident) = &r.type_name {
                    &*ident.sym == name
                } else {
                    false
                }
            }
            Type::Interface(i) => i.name == name,
            _ => false,
        }
    }

    /// Returns true if `self` is a `string` or a string literal.
    pub fn is_str(&self) -> bool {
        matches!(
            self.normalize(),
            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsStringKeyword,
                ..
            }) | Type::Lit(LitType { lit: RTsLit::Str(..), .. })
        )
    }

    pub fn is_str_lit(&self) -> bool {
        matches!(self.normalize(), Type::Lit(LitType { lit: RTsLit::Str(..), .. }))
    }

    pub fn is_bool_lit(&self) -> bool {
        matches!(self.normalize(), Type::Lit(LitType { lit: RTsLit::Bool(..), .. }))
    }

    pub fn is_num(&self) -> bool {
        matches!(
            self.normalize(),
            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsNumberKeyword,
                ..
            }) | Type::Lit(LitType {
                lit: RTsLit::Number(..),
                ..
            })
        )
    }

    pub fn is_bigint(&self) -> bool {
        matches!(
            self.normalize_instance(),
            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsBigIntKeyword,
                ..
            }) | Type::Lit(LitType {
                lit: RTsLit::BigInt(..),
                ..
            })
        )
    }

    pub fn is_bigint_lit(&self) -> bool {
        matches!(
            self.normalize(),
            Type::Lit(LitType {
                lit: RTsLit::BigInt(..),
                ..
            })
        )
    }

    pub fn is_num_lit(&self) -> bool {
        matches!(
            self.normalize(),
            Type::Lit(LitType {
                lit: RTsLit::Number(..),
                ..
            })
        )
    }

    /// Returns true if `self` is a `boolean` or a boolean literal.
    pub fn is_bool(&self) -> bool {
        matches!(
            self.normalize(),
            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsBooleanKeyword,
                ..
            }) | Type::Lit(LitType { lit: RTsLit::Bool(..), .. })
        )
    }
}

//impl Type {
//    /// Converts `Type` into `Type`.
//    pub fn owned(self) -> Type {
//        unsafe { transmute::<Cow<'_, Type>, Type>(Cow::Owned(self)) }
//    }
//
//    /// Converts `Type` into `Type`.
//    #[inline]
//    pub fn static_cast(&self) -> Type {
//        unsafe { transmute::<Cow<'_, Type>, Type>(Cow::Borrowed(self))
// }    }
//}

//impl Interface {
//    pub fn into_static(self) -> Interface<'static> {
//        Interface {
//            span: self.span,
//            name: self.name,
//            type_params: self.type_params.map(|v| v),
//            extends: self.extends.into_iter().map(|v|
// v).collect(),            body: self.body.into_iter().map(|v|
// v).collect(),        }
//    }
//}

//impl TsExpr {
//    pub fn into_static(self) -> TsExpr<'static> {
//        TsExpr {
//            span: self.span,
//            expr: self.expr,
//            type_params: self.type_params.map(|v| v),
//        }
//    }
//}
//
//impl TypeElement<'static> {
//    /// Converts `TypeTypeElement<'static>` into `TypeTypeElement`.
//    #[inline]
//    pub fn static_cast(self) -> TypeElement {
//        unsafe { transmute::<TypeElement<'static>, TypeElement>(self) }
//    }
//}
//
//impl TypeElement {
//    pub fn into_static(self) -> TypeElement<'static> {
//        match self {
//            TypeElement::Call(call) => TypeElement::Call(call),
//            TypeElement::Constructor(c) =>
// TypeElement::Constructor(c),            TypeElement::Index(i)
// => TypeElement::Index(i),            TypeElement::Method(m) =>
// TypeElement::Method(m),            TypeElement::Property(p) =>
// TypeElement::Property(p),        }
//    }
//}
//
//impl TypeParamInstantiation {
//    pub fn into_static(self) -> TypeParamInstantiation<'static> {
//        TypeParamInstantiation {
//            span: self.span,
//            params: self.params.into_iter().map(static_type).collect(),
//        }
//    }
//}
//
//impl CallSignature {
//    pub fn into_static(self) -> CallSignature<'static> {
//        CallSignature {
//            span: self.span,
//            params: self.params,
//            type_params: self.type_params.map(|v| v),
//            ret_ty: self.ret_ty.map(static_type),
//        }
//    }
//}
//
//impl ConstructorSignature {
//    pub fn into_static(self) -> ConstructorSignature<'static> {
//        ConstructorSignature {
//            span: self.span,
//            params: self.params,
//            ret_ty: self.ret_ty.map(static_type),
//            type_params: self.type_params.map(|v| v),
//        }
//    }
//}
//
//impl IndexSignature {
//    pub fn into_static(self) -> IndexSignature<'static> {
//        IndexSignature {
//            span: self.span,
//            readonly: self.readonly,
//            params: self.params,
//            type_ann: self.type_ann.map(static_type),
//        }
//    }
//}
//
//impl MethodSignature {
//    pub fn into_static(self) -> MethodSignature<'static> {
//        MethodSignature {
//            span: self.span,
//            computed: self.computed,
//            optional: self.optional,
//            key: self.key,
//            params: self.params,
//            readonly: self.readonly,
//            ret_ty: self.ret_ty.map(static_type),
//            type_params: self.type_params.map(|v| v),
//        }
//    }
//}
//
//impl PropertySignature {
//    pub fn into_static(self) -> PropertySignature<'static> {
//        PropertySignature {
//            span: self.span,
//            computed: self.computed,
//            optional: self.optional,
//            key: self.key,
//            params: self.params,
//            readonly: self.readonly,
//            type_ann: self.type_ann.map(static_type),
//            type_params: self.type_params.map(|v| v),
//        }
//    }
//}
//
//impl TypeParam {
//    pub fn into_static(self) -> TypeParam<'static> {
//        TypeParam {
//            span: self.span,
//            name: self.name,
//            constraint: self.constraint.map(|v| box static_type(*v)),
//            default: self.default.map(|v| box static_type(*v)),
//        }
//    }
//}
//
//impl TypeParamDecl {
//    pub fn into_static(self) -> TypeParamDecl<'static> {
//        TypeParamDecl {
//            span: self.span,
//            params: self.params.into_iter().map(|v|
// v).collect(),        }
//    }
//}
//
//impl TypeLit {
//    pub fn into_static(self) -> TypeLit<'static> {
//        TypeLit {
//            span: self.span,
//            members: self.members.into_iter().map(|v|
// v).collect(),        }
//    }
//}
//
//impl Alias {
//    pub fn into_static(self) -> Alias<'static> {
//        Alias {
//            span: self.span,
//            type_params: self.type_params.map(|v| v),
//            ty: box static_type(*self.ty),
//        }
//    }
//}
//
//impl TypeParam {
//    pub fn into_static(self) -> TypeParam<'static> {
//        TypeParam {
//            span: self.span,
//            name: self.name,
//            constraint: self.constraint.map(|v| box static_type(*v)),
//            default: self.default.map(|v| box static_type(*v)),
//        }
//    }
//}
//
//impl Tuple {
//    pub fn into_static(self) -> Tuple<'static> {
//        Tuple {
//            span: self.span,
//            types: self.types.into_iter().map(static_type).collect(),
//        }
//    }
//}
//
//impl Conditional {
//    pub fn into_static(self) -> Conditional<'static> {
//        Conditional {
//            span: self.span,
//            check_type: box static_type(*self.check_type),
//            extends_type: box static_type(*self.extends_type),
//            true_type: box static_type(*self.true_type),
//            false_type: box static_type(*self.false_type),
//        }
//    }
//}
//
//impl Mapped {
//    pub fn into_static(self) -> Mapped<'static> {
//        Mapped {
//            span: self.span,
//            readonly: self.readonly,
//            optional: self.optional,
//            type_param: self.type_param,
//            ty: self.ty.map(|ty| box static_type(*ty)),
//        }
//    }
//}
//
//impl Operator {
//    pub fn into_static(self) -> Operator<'static> {
//        Operator {
//            span: self.span,
//            op: self.op,
//            ty: box static_type(*self.ty),
//        }
//    }
//}
//
//impl Class {
//    pub fn into_static(self) -> Class<'static> {
//        Class {
//            span: self.span,
//            is_abstract: self.is_abstract,
//            name: self.name,
//            body: self.body.into_iter().map(|v| v).collect(),
//            type_params: self.type_params.map(|v| v),
//            super_class: self.super_class.map(|v| box static_type(*v)),
//            // implements: map_types(self.implements, static_type),
//        }
//    }
//}
//
//impl ClassInstance {
//    pub fn into_static(self) -> ClassInstance<'static> {
//        ClassInstance {
//            span: self.span,
//            cls: self.cls,
//            type_args: self.type_args.map(|v| v),
//        }
//    }
//}
//
//impl ClassMember {
//    pub fn into_static(self) -> ClassMember<'static> {
//        match self {
//            ClassMember::Constructor(v) =>
// ClassMember::Constructor(v),            ClassMember::Method(v)
// => ClassMember::Method(v),            ClassMember::Property(v)
// => ClassMember::Property(v),            ClassMember::IndexSignature(v) =>
// ClassMember::IndexSignature(v),        }
//    }
//}
//
//impl Constructor {
//    pub fn into_static(self) -> Constructor<'static> {
//        Constructor {
//            span: self.span,
//            params: self.params,
//            type_params: self.type_params.map(|v| v),
//            ret_ty: self.ret_ty.map(|v| box Cow::Owned(v)),
//        }
//    }
//}
//
//impl TypeElement {
//    pub fn key(&self) -> Option<&RExpr> {
//        static CONSTRUCTOR_EXPR: RExpr =
//            { RExpr::RIdent(RIdent::new(js_word!("constructor"), DUMMY_SP)) };
//
//        match *self {
//            TypeElement::Call(..) => None,
//            TypeElement::Constructor(..) => Some(&CONSTRUCTOR_EXPR),
//            TypeElement::Index(..) => None,
//            TypeElement::Method(ref el) => Some(&el.key),
//            TypeElement::Property(ref el) => Some(&el.key),
//        }
//    }
//}

impl VisitMut<Type> for Freezer {
    fn visit_mut(&mut self, ty: &mut Type) {
        if ty.is_clone_cheap() {
            return;
        }

        ty.assert_valid();

        ty.visit_mut_children_with(self);

        let new_ty = replace(
            ty,
            Type::Keyword(KeywordType {
                span: DUMMY_SP,
                kind: TsKeywordTypeKind::TsAnyKeyword,
                metadata: Default::default(),
                tracker: Default::default(),
            }),
        );

        *ty = Type::Arc(Freezed { ty: Arc::new(new_ty) })
    }
}

impl Type {
    pub fn as_bool(&self) -> Value<bool> {
        match self {
            Type::Arc(ref ty) => ty.ty.as_bool(),

            Type::Class(_) | Type::TypeLit(_) => Known(true),

            Type::Lit(ty) => Known(match &ty.lit {
                RTsLit::Number(v) => v.value != 0.0,
                RTsLit::Str(v) => v.value != *"",
                RTsLit::Tpl(v) => v.quasis.first().unwrap().raw != *"",
                RTsLit::Bool(v) => v.value,
                RTsLit::BigInt(v) => *v.value != BigInt::zero(),
            }),
            Type::Keyword(KeywordType { kind, .. }) => Known(match kind {
                TsKeywordTypeKind::TsNeverKeyword
                | TsKeywordTypeKind::TsStringKeyword
                | TsKeywordTypeKind::TsNumberKeyword
                | TsKeywordTypeKind::TsUnknownKeyword
                | TsKeywordTypeKind::TsBooleanKeyword
                | TsKeywordTypeKind::TsAnyKeyword
                | TsKeywordTypeKind::TsIntrinsicKeyword => return Unknown,
                TsKeywordTypeKind::TsSymbolKeyword | TsKeywordTypeKind::TsBigIntKeyword | TsKeywordTypeKind::TsObjectKeyword => true,

                TsKeywordTypeKind::TsUndefinedKeyword | TsKeywordTypeKind::TsNullKeyword | TsKeywordTypeKind::TsVoidKeyword => false,
            }),

            _ => Unknown,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct StaticThis {
    pub span: Span,
    pub metadata: StaticThisMetadata,

    pub tracker: Tracker<"StaticThis">,
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(StaticThis, [u8; 24]);

#[derive(Debug, Clone, PartialEq, Eq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct ThisType {
    pub span: Span,
    pub metadata: ThisTypeMetadata,

    pub tracker: Tracker<"ThisType">,
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(ThisType, [u8; 24]);

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct TplType {
    pub span: Span,

    pub quasis: Vec<TplElem>,
    pub types: Vec<Type>,

    pub metadata: TplTypeMetadata,

    pub tracker: Tracker<"TplType">,
}

#[derive(Debug, Clone, PartialEq, Eq, Spanned, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct TplElem {
    pub span: Span,

    pub value: Atom,
}

impl From<&'_ RTplElement> for TplElem {
    fn from(v: &RTplElement) -> Self {
        TplElem {
            span: v.span,
            value: v.cooked.clone().unwrap(),
        }
    }
}

impl From<RTplElement> for TplElem {
    fn from(v: RTplElement) -> Self {
        TplElem {
            span: v.span,
            value: v.cooked.unwrap(),
        }
    }
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(TplType, [u8; 72]);

#[derive(Clone, PartialEq, EqIgnoreSpan, TypeEq, Serialize, Deserialize)]
pub struct Freezed {
    ty: Arc<Type>,
}

impl Debug for Freezed {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.ty)
    }
}

impl Spanned for Freezed {
    fn span(&self) -> Span {
        self.ty.span()
    }
}

#[cfg(target_pointer_width = "64")]
assert_eq_size!(Freezed, [u8; 8]);

impl Visitable for Freezed {}

impl<V> VisitWith<V> for Freezed
where
    V: ?Sized,
{
    #[inline]
    fn visit_children_with(&self, visitor: &mut V) {
        self.ty.visit_with(visitor);
    }
}

impl<V> VisitMutWith<V> for Freezed
where
    V: ?Sized,
{
    #[inline]
    fn visit_mut_children_with(&mut self, _v: &mut V) {
        unreachable!()
    }
}

impl<V> FoldWith<V> for Freezed
where
    V: ?Sized,
{
    #[inline]
    fn fold_children_with(self, _v: &mut V) -> Self {
        unreachable!()
    }
}

/// Getter and setter.
///
/// ## `getter = false`, `setter = false`
///
/// Property declared without getter or setter.
///
/// # Notes
///
/// [TypeEq] and [EqIgnoreSpan] always return true because this struct is
/// metadata.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Visit, Serialize, Deserialize)]
pub struct Accessor {
    pub getter: bool,
    pub setter: bool,
}

impl EqIgnoreSpan for Accessor {
    fn eq_ignore_span(&self, _: &Self) -> bool {
        true
    }
}

impl TypeEq for Accessor {
    fn type_eq(&self, _: &Self) -> bool {
        true
    }
}

pub trait Valid: Sized + VisitWith<ValidityChecker> {
    #[instrument(skip_all)]
    fn is_valid(&self) -> bool {
        let mut v = ValidityChecker { valid: true };
        self.visit_with(&mut v);
        v.valid
    }
}

impl Valid for Type {}

impl Valid for Intersection {}

impl Valid for Union {}

pub struct ValidityChecker {
    valid: bool,
}

impl Visit<Type> for ValidityChecker {
    fn visit(&mut self, ty: &Type) {
        // Freezed types are valid.
        if matches!(ty, Type::Arc(..)) {
            return;
        }

        ty.visit_children_with(self);
    }
}

impl Visit<Union> for ValidityChecker {
    fn visit(&mut self, ty: &Union) {
        for (i, t1) in ty.types.iter().enumerate() {
            for (j, t2) in ty.types.iter().enumerate() {
                if i == j {
                    continue;
                }
                if t1.type_eq(t2) {
                    self.valid = false;
                    return;
                }
            }
        }

        if ty.types.len() <= 1 {
            self.valid = false;
            return;
        }

        if ty.types.iter().any(|t| t.is_union_type()) {
            self.valid = false;
            return;
        }

        ty.visit_children_with(self);
    }
}

impl Visit<Intersection> for ValidityChecker {
    fn visit(&mut self, ty: &Intersection) {
        for (i, t1) in ty.types.iter().enumerate() {
            for (j, t2) in ty.types.iter().enumerate() {
                if i == j {
                    continue;
                }
                if t1.type_eq(t2) {
                    self.valid = false;
                    return;
                }
            }
        }

        if ty.types.len() <= 1 {
            self.valid = false;
            return;
        }

        if ty.types.iter().any(|t| t.is_intersection()) {
            self.valid = false;
            return;
        }

        ty.visit_children_with(self);
    }
}

struct CheckCheapClone {
    cheap: bool,
}

impl Visit<Type> for CheckCheapClone {
    fn visit(&mut self, ty: &Type) {
        if ty.is_clone_cheap() {
            return;
        }

        self.cheap = false;
    }
}

macro_rules! impl_freeze {
    ($T:ty) => {
        impl Freeze for $T {
            fn is_clone_cheap(&self) -> bool {
                let mut v = CheckCheapClone { cheap: true };
                self.visit_with(&mut v);
                v.cheap
            }

            #[inline]
            #[instrument(skip_all)]
            fn freeze(&mut self) {
                self.visit_mut_with(&mut Freezer);
            }
        }
    };
}

impl_freeze!(FnParam);
impl_freeze!(Interface);
impl_freeze!(TsExpr);
impl_freeze!(Type);
impl_freeze!(TypeElement);
impl_freeze!(TypeParam);
impl_freeze!(TypeParamDecl);
impl_freeze!(TypeParamInstantiation);
impl_freeze!(TypeOrSpread);
impl_freeze!(Key);
impl_freeze!(Enum);
impl_freeze!(ClassDef);
impl_freeze!(Mapped);
