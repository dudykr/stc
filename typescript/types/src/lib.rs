//! This crate exists to reduce compile time.
//!
//! The visitor is too slow to compile everytime I make change.
#![deny(unused)]
#![allow(incomplete_features)]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(specialization)]

use self::type_id::SymbolId;
pub use self::{
    convert::rprop_name_to_expr,
    id::Id,
    intrinsic::{Intrinsic, IntrinsicKind},
    metadata::*,
    module_id::ModuleId,
};
use fxhash::FxHashMap;
use is_macro::Is;
use num_bigint::BigInt;
use num_traits::Zero;
use rnode::{FoldWith, VisitMutWith, VisitWith};
use scoped_tls::scoped_thread_local;
use static_assertions::assert_eq_size;
use stc_arc_cow::{Arc, BoxedArcCow};
use stc_ts_ast_rnode::{
    RBigInt, RExpr, RIdent, RNumber, RPat, RPrivateName, RStr, RTplElement, RTsEntityName, RTsEnumMemberId,
    RTsKeywordType, RTsLit, RTsModuleName, RTsNamespaceDecl, RTsThisType, RTsThisTypeOrIdent,
};
use stc_utils::{
    cache::{Freeze, ALLOW_DEEP_CLONE},
    debug_ctx,
    ext::TypeVecExt,
    panic_ctx,
};
use stc_visit::{Visit, Visitable};
use std::{
    self,
    borrow::Cow,
    fmt,
    fmt::{Debug, Formatter},
    iter::FusedIterator,
    ops::AddAssign,
};
use swc_atoms::{js_word, JsWord};
use swc_common::{EqIgnoreSpan, FromVariant, Span, Spanned, SyntaxContext, TypeEq, DUMMY_SP};
use swc_ecma_ast::{Accessibility, TruePlusMinus, TsKeywordTypeKind, TsTypeOperatorOp};
use swc_ecma_utils::{
    Value,
    Value::{Known, Unknown},
};
use tracing::instrument;

mod convert;
mod id;
mod intrinsic;
pub mod macros;
mod metadata;
pub mod module_id;
pub mod name;
pub mod type_id;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IdCtx {
    Var,
    Type,
}

#[derive(Debug, Clone, Default, PartialEq)]
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

/// This type is expected to stored in a [Box], like `Vec<Type>`.
#[derive(Debug, PartialEq, Spanned, FromVariant, Is, EqIgnoreSpan, TypeEq, Visit)]
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

    #[is(name = "ref_type")]
    Ref(Ref),
    TypeLit(TypeLit),
    Keyword(KeywordType),
    Conditional(Conditional),
    Tuple(Tuple),
    Array(Array),
    #[is(name = "union_type")]
    Union(Union),
    #[is(name = "intersection_type")]
    Intersection(Intersection),
    Function(Function),
    Constructor(Constructor),

    Operator(Operator),

    #[is(name = "type_param")]
    Param(TypeParam),
    EnumVariant(EnumVariant),

    Interface(Interface),

    #[is(name = "enum_type")]
    Enum(Enum),

    Mapped(Mapped),

    /// export type A<B> = Foo<B>;
    Alias(Alias),
    Namespace(RTsNamespaceDecl),
    Module(Module),

    /// Instance of a class.
    Class(Class),

    /// Class definition itself.
    ClassDef(ClassDef),

    Rest(RestType),

    Optional(OptionalType),

    Symbol(Symbol),

    Tpl(TplType),

    Intrinsic(Intrinsic),
}

impl Clone for Type {
    #[instrument(name = "Type::clone", skip(self))]
    fn clone(&self) -> Self {
        match self {
            Type::Keyword(ty) => ty.clone().into(),
            Type::StaticThis(ty) => ty.clone().into(),
            Type::This(ty) => ty.clone().into(),
            Type::Symbol(ty) => ty.clone().into(),

            _ => {
                scoped_thread_local!(static DEEP: ());

                macro_rules! work {
                    () => {{
                        match self {
                            Type::Intrinsic(ty) => ty.clone().into(),
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
                            Type::Operator(ty) => ty.clone().into(),
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

                if cfg!(debug_assertions)
                    && self.span() != DUMMY_SP
                    && !self.is_clone_cheap()
                    && !ALLOW_DEEP_CLONE.is_set()
                {
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

assert_eq_size!(Type, [u8; 112]);

fn _assert_send_sync() {
    fn assert<T: Send + Sync>() {}

    assert::<Type>();
    assert::<StaticThis>();
    assert::<RTsThisType>();
    assert::<QueryType>();
    assert::<InferType>();
    assert::<ImportType>();
    assert::<Predicate>();
    assert::<IndexedAccessType>();

    assert::<Ref>();
    assert::<TypeLit>();
    assert::<RTsKeywordType>();
    assert::<Conditional>();
    assert::<Tuple>();
    assert::<Array>();
    assert::<Union>();
    assert::<Intersection>();
    assert::<Function>();
    assert::<Constructor>();

    assert::<Operator>();

    assert::<TypeParam>();
    assert::<EnumVariant>();
    assert::<Interface>();
    assert::<Enum>();

    assert::<Mapped>();
    assert::<Alias>();
    assert::<RTsNamespaceDecl>();
    assert::<Module>();

    assert::<Class>();
    assert::<ClassDef>();

    assert::<RestType>();
    assert::<OptionalType>();
    assert::<Symbol>();
}

#[derive(Debug, Clone, PartialEq, EqIgnoreSpan, TypeEq, Visit, Is, Spanned)]
pub enum Key {
    Computed(ComputedKey),
    Normal { span: Span, sym: JsWord },
    Num(#[use_eq_ignore_span] RNumber),
    BigInt(#[use_eq_ignore_span] RBigInt),
    Private(#[use_eq_ignore_span] PrivateName),
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

#[derive(Debug, Clone, PartialEq, Eq, EqIgnoreSpan, TypeEq, Visit, Spanned)]
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

assert_eq_size!(Key, [u8; 56]);

impl Key {
    pub fn ty(&self) -> Cow<Type> {
        match self {
            Key::Computed(prop) => Cow::Borrowed(&*prop.ty),
            Key::Normal { span, sym } => Cow::Owned(Type::Lit(LitType {
                span: *span,
                lit: RTsLit::Str(RStr {
                    span: *span,
                    value: sym.clone(),
                    has_escape: false,
                    kind: Default::default(),
                }),
                metadata: Default::default(),
            })),
            Key::Num(n) => Cow::Owned(Type::Lit(LitType {
                span: n.span,
                lit: RTsLit::Number(n.clone()),
                metadata: Default::default(),
            })),
            Key::BigInt(n) => Cow::Owned(Type::Lit(LitType {
                span: n.span,
                lit: RTsLit::BigInt(n.clone()),
                metadata: Default::default(),
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

#[derive(Debug, Clone, PartialEq, EqIgnoreSpan, TypeEq, Visit, Spanned)]
pub struct ComputedKey {
    pub span: Span,
    pub expr: Box<RExpr>,
    pub ty: BoxedArcCow<Type>,
}

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
#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Instance {
    pub span: Span,
    pub ty: BoxedArcCow<Type>,
    pub metadata: InstanceMetadata,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct LitType {
    pub span: Span,

    #[use_eq_ignore_span]
    pub lit: RTsLit,
    pub metadata: LitTypeMetadata,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct KeywordType {
    pub span: Span,

    #[use_eq_ignore_span]
    pub kind: TsKeywordTypeKind,
    pub metadata: KeywordTypeMetadata,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Symbol {
    pub span: Span,
    pub id: SymbolId,
    pub metadata: SymbolMetadata,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct RestType {
    pub span: Span,
    pub ty: BoxedArcCow<Type>,
    pub metadata: RestTypeMetadata,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct OptionalType {
    pub span: Span,
    pub ty: BoxedArcCow<Type>,
    pub metadata: OptionalTypeMetadata,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct IndexedAccessType {
    pub span: Span,
    pub readonly: bool,
    pub obj_type: BoxedArcCow<Type>,
    pub index_type: BoxedArcCow<Type>,
    pub metadata: IndexedAccessTypeMetadata,
}

#[derive(Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Ref {
    pub span: Span,
    /// Id of the module where the ref is used in.
    pub ctxt: ModuleId,
    #[use_eq_ignore_span]
    pub type_name: RTsEntityName,
    pub type_args: Option<Box<TypeParamInstantiation>>,
    pub metadata: RefMetadata,
}

impl Debug for Ref {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        if let Some(type_args) = &self.type_args {
            write!(f, "{:?}<{:?}>", self.type_name, type_args)
        } else {
            write!(f, "{:?}", self.type_name)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct InferType {
    pub span: Span,
    pub type_param: TypeParam,
    pub metadata: InferTypeMetadata,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct QueryType {
    pub span: Span,
    pub expr: Box<QueryExpr>,
    pub metadata: QueryTypeMetdata,
}

assert_eq_size!(QueryType, [u8; 32]);

#[derive(Debug, Clone, PartialEq, Spanned, FromVariant, EqIgnoreSpan, TypeEq, Visit)]
pub enum QueryExpr {
    TsEntityName(#[use_eq_ignore_span] RTsEntityName),
    Import(ImportType),
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct ImportType {
    pub span: Span,
    pub arg: RStr,
    #[use_eq_ignore_span]
    pub qualifier: Option<RTsEntityName>,
    pub type_params: Option<Box<TypeParamInstantiation>>,
    pub metadata: ImportTypeMetadata,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Module {
    pub span: Span,
    #[use_eq_ignore_span]
    pub name: RTsModuleName,
    pub exports: Box<ModuleTypeData>,
    pub metadata: ModuleTypeMetadata,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Enum {
    pub span: Span,
    pub declare: bool,
    pub is_const: bool,
    #[use_eq_ignore_span]
    pub id: RIdent,
    pub members: Vec<EnumMember>,
    pub has_num: bool,
    pub has_str: bool,

    pub metadata: EnumMetadata,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct EnumMember {
    pub span: Span,
    #[use_eq_ignore_span]
    pub id: RTsEnumMemberId,
    #[use_eq_ignore_span]
    pub val: Box<RExpr>,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Class {
    pub span: Span,
    pub def: Box<ClassDef>,
    pub metadata: ClassMetadata,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct ClassDef {
    pub span: Span,
    pub is_abstract: bool,
    pub name: Option<Id>,
    pub super_class: Option<BoxedArcCow<Type>>,
    pub body: Vec<ClassMember>,
    pub type_params: Option<Box<TypeParamDecl>>,
    pub implements: Box<Vec<TsExpr>>,
    pub metadata: ClassDefMetadata,
}

#[derive(Debug, Clone, PartialEq, Spanned, FromVariant, EqIgnoreSpan, TypeEq, Visit, Is)]
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
#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
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
    pub ret_ty: BoxedArcCow<Type>,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct ClassProperty {
    pub span: Span,
    #[use_eq_ignore_span]
    pub key: Key,
    pub value: Option<BoxedArcCow<Type>>,
    pub is_static: bool,
    #[use_eq]
    pub accessibility: Option<Accessibility>,
    pub is_abstract: bool,
    pub is_optional: bool,
    pub readonly: bool,
    pub definite: bool,

    pub accessor: Accessor,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Mapped {
    pub span: Span,
    #[use_eq]
    pub readonly: Option<TruePlusMinus>,
    #[use_eq]
    pub optional: Option<TruePlusMinus>,
    pub name_type: Option<BoxedArcCow<Type>>,
    pub type_param: Box<TypeParam>,
    pub ty: Option<BoxedArcCow<Type>>,
    pub metadata: MappedMetadata,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Conditional {
    pub span: Span,
    pub check_type: BoxedArcCow<Type>,
    pub extends_type: BoxedArcCow<Type>,
    pub true_type: BoxedArcCow<Type>,
    pub false_type: BoxedArcCow<Type>,
    pub metadata: ConditionalMetadata,
}

/// TODO(kdy1): Remove this and create `keyof`, `unique` and `readonly` types.
#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Operator {
    pub span: Span,
    #[use_eq]
    pub op: TsTypeOperatorOp,
    pub ty: BoxedArcCow<Type>,
    pub metadata: OperatorMetadata,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Tuple {
    pub span: Span,
    pub elems: Vec<TupleElement>,
    pub metadata: TupleMetadata,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct TupleElement {
    pub span: Span,
    #[not_type]
    pub label: Option<RPat>,
    pub ty: BoxedArcCow<Type>,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Alias {
    pub span: Span,
    pub type_params: Option<Box<TypeParamDecl>>,
    pub ty: BoxedArcCow<Type>,
    pub metadata: AliasMetadata,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Interface {
    pub span: Span,
    pub name: Id,
    pub type_params: Option<Box<TypeParamDecl>>,
    pub extends: Vec<TsExpr>,
    pub body: Vec<TypeElement>,
    pub metadata: InterfaceMetadata,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct TypeLit {
    pub span: Span,
    pub members: Vec<TypeElement>,
    pub metadata: TypeLitMetadata,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct TypeParamDecl {
    pub span: Span,
    pub params: Vec<TypeParam>,
}

/// Typescript expression with type arguments
#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct TsExpr {
    pub span: Span,
    #[use_eq_ignore_span]
    pub expr: RTsEntityName,
    pub type_args: Option<Box<TypeParamInstantiation>>,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct TypeParamInstantiation {
    pub span: Span,

    /// TODO(kdy1): Rename to `args`.
    pub params: Vec<BoxedArcCow<Type>>,
}

#[derive(Debug, Clone, PartialEq, Spanned, FromVariant, EqIgnoreSpan, TypeEq, Visit, Is)]
pub enum TypeElement {
    Call(CallSignature),
    Constructor(ConstructorSignature),
    Property(PropertySignature),
    Method(MethodSignature),
    Index(IndexSignature),
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
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct CallSignature {
    pub span: Span,
    pub params: Vec<FnParam>,
    pub type_params: Option<TypeParamDecl>,
    pub ret_ty: Option<BoxedArcCow<Type>>,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct ConstructorSignature {
    pub span: Span,
    /// Only for synthesized type elements.
    #[use_eq]
    pub accessibility: Option<Accessibility>,
    pub params: Vec<FnParam>,
    pub ret_ty: Option<BoxedArcCow<Type>>,
    pub type_params: Option<TypeParamDecl>,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct PropertySignature {
    pub span: Span,
    /// Only for synthesized type elements.
    #[use_eq]
    pub accessibility: Option<Accessibility>,
    pub readonly: bool,
    pub key: Key,
    pub optional: bool,
    pub params: Vec<FnParam>,
    pub type_ann: Option<BoxedArcCow<Type>>,
    pub type_params: Option<TypeParamDecl>,
    pub metadata: TypeElMetadata,

    pub accessor: Accessor,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct MethodSignature {
    pub span: Span,
    /// Only for synthesized type elements.
    #[use_eq]
    pub accessibility: Option<Accessibility>,
    pub readonly: bool,
    pub key: Key,
    pub optional: bool,
    pub params: Vec<FnParam>,
    pub ret_ty: Option<BoxedArcCow<Type>>,
    pub type_params: Option<TypeParamDecl>,
    pub metadata: TypeElMetadata,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct IndexSignature {
    pub params: Vec<FnParam>,
    pub type_ann: Option<BoxedArcCow<Type>>,

    pub readonly: bool,
    pub span: Span,

    pub is_static: bool,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Array {
    pub span: Span,
    pub elem_type: BoxedArcCow<Type>,
    pub metadata: ArrayMetadata,
}

/// a | b
#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Union {
    pub span: Span,
    pub types: Vec<BoxedArcCow<Type>>,
    pub metadata: UnionMetadata,
}

impl Union {
    pub fn assert_valid(&self) {
        if !cfg!(debug_assertions) {
            return;
        }

        self.types.iter().for_each(|ty| ty.assert_valid());

        for (i, t1) in self.types.iter().enumerate() {
            for (j, t2) in self.types.iter().enumerate() {
                if i == j {
                    continue;
                }
                if t1.type_eq(t2) {
                    unreachable!("[INVALID_TYPE]: A union type has duplicate elements: ({:?})", t1)
                }
            }
        }

        if self.types.len() <= 1 {
            unreachable!(
                "[INVALID_TYPE]: A union type should have multiple items. Got {:?}",
                self.types
            );
        }
    }
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct FnParam {
    pub span: Span,
    pub required: bool,
    #[not_type]
    pub pat: RPat,
    pub ty: BoxedArcCow<Type>,
}

/// a & b
#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Intersection {
    pub span: Span,
    pub types: Vec<BoxedArcCow<Type>>,
    pub metadata: IntersectionMetadata,
}

assert_eq_size!(Intersection, [u8; 48]);

impl Intersection {
    pub fn assert_valid(&self) {
        if !cfg!(debug_assertions) {
            return;
        }

        self.types.iter().for_each(|ty| ty.assert_valid());

        for (i, t1) in self.types.iter().enumerate() {
            for (j, t2) in self.types.iter().enumerate() {
                if i == j {
                    continue;
                }
                if t1.type_eq(t2) {
                    unreachable!(
                        "[INVALID_TYPE]: An intersection type has duplicate elements: ({:?})",
                        t1
                    )
                }
            }
        }

        if self.types.len() <= 1 {
            unreachable!(
                "[INVALID_TYPE]: An intersection type should have multiple items. Got {:?}",
                self.types
            );
        }
    }
}

/// A type parameter
#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct TypeParam {
    pub span: Span,
    pub name: Id,
    pub constraint: Option<BoxedArcCow<Type>>,
    pub default: Option<BoxedArcCow<Type>>,
    pub metadata: TypeParamMetadata,
}

/// FooEnum.A
#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct EnumVariant {
    pub span: Span,
    pub ctxt: ModuleId,
    pub enum_name: Id,
    /// [None] if for the general instance type of an enum.
    pub name: Option<JsWord>,
    pub metadata: EnumVariantMetadata,
}

assert_eq_size!(EnumVariant, [u8; 56]);

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Function {
    pub span: Span,
    pub type_params: Option<TypeParamDecl>,
    pub params: Vec<FnParam>,
    pub ret_ty: BoxedArcCow<Type>,
    pub metadata: FunctionMetadata,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Constructor {
    pub span: Span,
    pub type_params: Option<TypeParamDecl>,
    pub params: Vec<FnParam>,
    /// The return type.
    pub type_ann: BoxedArcCow<Type>,
    pub is_abstract: bool,
    pub metadata: ConstructorMetadata,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Predicate {
    pub span: Span,
    #[use_eq_ignore_span]
    pub param_name: RTsThisTypeOrIdent,
    pub asserts: bool,
    pub ty: Option<BoxedArcCow<Type>>,
    pub metadata: PredicateMetadata,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct TypeOrSpread {
    pub span: Span,
    pub spread: Option<Span>,
    pub ty: BoxedArcCow<Type>,
}

pub trait TypeIterExt {}

struct AssertCloneCheap;

impl Visit<Type> for AssertCloneCheap {
    fn visit(&mut self, ty: &Type) {
        debug_assert!(ty.is_clone_cheap(), "{:?} is not cheap to clone", ty);
    }
}

impl Type {
    #[cfg_attr(not(debug_assertions), inline(always))]
    pub fn assert_clone_cheap(&self) {
        if !cfg!(debug_assertions) {
            return;
        }

        let _ctx = debug_ctx!(format!("assert_clone_cheap: {:?}", self));

        self.visit_with(&mut AssertCloneCheap);
    }

    pub fn intersection<I>(span: Span, iter: I) -> BoxedArcCow<Self>
    where
        I: IntoIterator<Item = BoxedArcCow<Type>>,
    {
        let mut tys = vec![];

        for ty in iter {
            if ty.is_intersection_type() {
                tys.extend(ty.into_inner().expect_intersection_type().types);
            } else {
                tys.push(ty);
            }
        }
        tys.dedup_type();

        let has_str = tys.iter().any(|ty| ty.is_str());
        // TODO
        let has_bool = tys.iter().any(|ty| ty.is_kwd(TsKeywordTypeKind::TsBooleanKeyword));
        let has_num = tys.iter().any(|ty| ty.is_num());

        if (has_str && has_bool) || (has_bool && has_num) || (has_num && has_str) {
            return Type::never(span, Default::default()).into();
        }

        if tys.iter().any(|ty| ty.is_never()) {
            return Type::never(span, Default::default()).into();
        }

        match tys.len() {
            0 => Type::never(span, Default::default()).into(),
            1 => tys.into_iter().next().unwrap(),
            _ => Type::Intersection(Intersection {
                span,
                types: tys,
                metadata: Default::default(),
            })
            .into(),
        }
    }

    pub fn new_union_without_dedup(span: Span, types: Vec<BoxedArcCow<Type>>) -> BoxedArcCow<Self> {
        let ty = match types.len() {
            0 => Type::never(span, Default::default()).into(),
            1 => types.into_iter().next().unwrap(),
            _ => Type::Union(Union {
                span,
                types,
                metadata: Default::default(),
            })
            .into(),
        };
        ty.assert_valid();
        ty
    }

    pub fn new_union<I: IntoIterator<Item = BoxedArcCow<Self>> + Debug>(span: Span, iter: I) -> BoxedArcCow<Self> {
        let _ctx = debug_ctx!(format!("Iterator: {:?}", iter));

        let mut elements: Vec<BoxedArcCow<Type>> = vec![];

        for ty in iter {
            if ty.is_union_type() {
                let types = ty.into_inner().expect_union_type().types;
                for new in types {
                    if elements.iter().any(|prev| prev.type_eq(&new)) {
                        continue;
                    }
                    elements.push(new)
                }
            } else {
                if elements.iter().any(|prev| prev.type_eq(&ty)) {
                    continue;
                }
                elements.push(ty)
            }
        }
        // Drop `never`s.
        elements.retain(|ty| !ty.is_never());

        Self::new_union_without_dedup(span, elements)
    }

    /// Creates a new type from `iter`.
    ///
    /// Note:
    ///
    ///  - never types are excluded.
    pub fn union<I: IntoIterator<Item = BoxedArcCow<Self>> + Debug>(iter: I) -> BoxedArcCow<Self> {
        let _ctx = debug_ctx!(format!("Iterator: {:?}", iter));

        let mut span = DUMMY_SP;

        let mut elements: Vec<BoxedArcCow<Type>> = vec![];

        for ty in iter {
            let sp = ty.span();

            if sp.lo() < span.lo() {
                span = span.with_lo(sp.lo());
            }
            if sp.hi() > span.hi() {
                span = span.with_hi(sp.hi());
            }

            if ty.is_union_type() {
                let types = ty.into_inner().expect_union_type().types;
                for new in types {
                    if elements.iter().any(|prev| prev.type_eq(&new)) {
                        continue;
                    }
                    elements.push(new)
                }
            } else {
                if elements.iter().any(|prev| prev.type_eq(&ty)) {
                    continue;
                }
                elements.push(ty)
            }
        }
        // Drop `never`s.
        elements.retain(|ty| !ty.is_never());

        let ty = match elements.len() {
            0 => Type::never(span, Default::default()).into(),
            1 => elements.into_iter().next().unwrap(),
            _ => Type::Union(Union {
                span,
                types: elements,
                metadata: Default::default(),
            })
            .into(),
        };
        ty.assert_valid();
        ty
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
            }),
            _ => self,
        }
    }

    pub fn contains_void(&self) -> bool {
        match self {
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
        match self {
            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsAnyKeyword,
                ..
            }) => true,

            Type::Union(t) => t.types.iter().any(|t| t.is_any()),

            Type::Instance(ty) => ty.ty.is_any(),

            _ => false,
        }
    }

    pub fn is_unknown(&self) -> bool {
        match self {
            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsUnknownKeyword,
                ..
            }) => true,

            Type::Union(t) => t.types.iter().any(|t| t.is_unknown()),

            _ => false,
        }
    }

    pub fn contains_undefined(&self) -> bool {
        match self {
            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsUndefinedKeyword,
                ..
            }) => true,

            Type::Union(ref t) => t.types.iter().any(|t| t.contains_undefined()),

            _ => false,
        }
    }
}

impl Type {
    /// TODO
    pub fn is_clone_cheap(&self) -> bool {
        match self {
            Type::Keyword(..) | Type::Lit(..) | Type::This(..) | Type::StaticThis(..) | Type::Symbol(..) => true,

            // TODO(kdy1): Make this false.
            Type::Param(TypeParam {
                constraint, default, ..
            }) => {
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
        match self {
            Type::Instance(ty) => ty.ty.is_kwd(k),
            Type::Keyword(KeywordType { kind, .. }) if *kind == k => true,
            _ => false,
        }
    }

    pub fn is_unique_symbol(&self) -> bool {
        match *self {
            Type::Operator(Operator {
                op: TsTypeOperatorOp::Unique,
                ref ty,
                ..
            }) => ty.is_kwd(TsKeywordTypeKind::TsSymbolKeyword),
            _ => false,
        }
    }

    pub fn is_never(&self) -> bool {
        self.is_kwd(TsKeywordTypeKind::TsNeverKeyword)
    }

    pub fn never<'any>(span: Span, metadata: KeywordTypeMetadata) -> Self {
        Type::Keyword(KeywordType {
            span,
            kind: TsKeywordTypeKind::TsNeverKeyword,
            metadata,
        })
    }

    pub fn undefined<'any>(span: Span, metadata: KeywordTypeMetadata) -> Self {
        Type::Keyword(KeywordType {
            span,
            kind: TsKeywordTypeKind::TsUndefinedKeyword,
            metadata,
        })
    }

    pub fn any<'any>(span: Span, metadata: KeywordTypeMetadata) -> Self {
        Type::Keyword(KeywordType {
            span,
            kind: TsKeywordTypeKind::TsAnyKeyword,
            metadata,
        })
    }

    pub fn void<'any>(span: Span, metadata: KeywordTypeMetadata) -> Self {
        Type::Keyword(KeywordType {
            span,
            kind: TsKeywordTypeKind::TsVoidKeyword,
            metadata,
        })
    }

    pub fn unknown<'any>(span: Span, metadata: KeywordTypeMetadata) -> Self {
        Type::Keyword(KeywordType {
            span,
            kind: TsKeywordTypeKind::TsUnknownKeyword,
            metadata,
        })
    }
}

impl Type {
    pub fn metadata(&self) -> CommonTypeMetadata {
        match self {
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
            Type::Operator(ty) => ty.metadata.common,
            Type::Param(ty) => ty.metadata.common,
            Type::EnumVariant(ty) => ty.metadata.common,
            Type::Interface(ty) => ty.metadata.common,
            Type::Enum(ty) => ty.metadata.common,
            Type::Mapped(ty) => ty.metadata.common,
            Type::Alias(ty) => ty.metadata.common,
            Type::Namespace(_ty) => todo!("Type::Namespace -> metadata()"),
            Type::Module(ty) => ty.metadata.common,
            Type::Class(ty) => ty.metadata.common,
            Type::ClassDef(ty) => ty.metadata.common,
            Type::Rest(ty) => ty.metadata.common,
            Type::Optional(ty) => ty.metadata.common,
            Type::Symbol(ty) => ty.metadata.common,
            Type::Tpl(ty) => ty.metadata.common,
            Type::Intrinsic(ty) => ty.metadata.common,
        }
    }

    pub fn metadata_mut(&mut self) -> &mut CommonTypeMetadata {
        match self {
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
            Type::Operator(ty) => &mut ty.metadata.common,
            Type::Param(ty) => &mut ty.metadata.common,
            Type::EnumVariant(ty) => &mut ty.metadata.common,
            Type::Interface(ty) => &mut ty.metadata.common,
            Type::Enum(ty) => &mut ty.metadata.common,
            Type::Mapped(ty) => &mut ty.metadata.common,
            Type::Alias(ty) => &mut ty.metadata.common,
            Type::Namespace(_ty) => todo!("Type::Namespace -> metadata()"),
            Type::Module(ty) => &mut ty.metadata.common,
            Type::Class(ty) => &mut ty.metadata.common,
            Type::ClassDef(ty) => &mut ty.metadata.common,
            Type::Rest(ty) => &mut ty.metadata.common,
            Type::Optional(ty) => &mut ty.metadata.common,
            Type::Symbol(ty) => &mut ty.metadata.common,
            Type::Tpl(ty) => &mut ty.metadata.common,
            Type::Intrinsic(ty) => &mut ty.metadata.common,
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

        match self {
            Type::Operator(ty) => ty.span = span,

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

            Type::Enum(e) => e.span = span,

            Type::EnumVariant(e) => e.span = span,

            Type::Interface(e) => e.span = span,

            Type::Alias(a) => a.span = span,

            Type::Namespace(n) => n.span = span,

            Type::Module(m) => m.span = span,

            Type::Class(c) => c.span = span,

            Type::ClassDef(c) => c.span = span,

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

            Type::Intrinsic(ty) => ty.span = span,
        }
    }
}

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

        ty.assert_valid();

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

        ty.assert_valid();

        for item in ty.types.iter() {
            if item.is_intersection_type() {
                unreachable!("[INVALID_TYPE]: An intersection type should not have an intersection item")
            }
        }
    }
}

impl Type {
    /// Panics if type is invalid.
    ///
    /// # Validity
    ///
    /// For example, `any | any` is invalid because
    /// union should not have duplicate elements.
    pub fn assert_valid(&self) {
        if !cfg!(debug_assertions) {
            return;
        }

        let _ctx = debug_ctx!(format!("{:?}", self));

        self.visit_with(&mut AssertValid);
    }

    pub fn is_global_this(&self) -> bool {
        match self {
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
    /// [Type::Arc] and [Type::Instance] are normalized.
    pub fn normalize_instance<'s, 'c>(&'s self) -> &'c Type
    where
        's: 'c,
    {
        match self {
            Type::Instance(ty) => ty.ty.normalize_instance(),
            _ => self,
        }
    }

    /// TODO(kdy1): Make this more efficient, and explode subunions.
    pub fn iter_union(&self) -> impl Debug + Iterator<Item = &Type> {
        Iter { ty: self, idx: 0 }
    }
}

#[derive(Debug)]
struct Iter<'a> {
    ty: &'a Type,
    idx: usize,
}

impl<'a> Iterator for Iter<'a> {
    type Item = &'a Type;

    fn next(&mut self) -> Option<Self::Item> {
        match self.ty {
            Type::Union(ref u) => {
                let ty = u.types.get(self.idx);
                self.idx += 1;
                return Some(&*ty?);
            }

            _ if self.idx == 0 => {
                self.idx = 1;
                Some(&self.ty)
            }

            _ => None,
        }
    }
}

impl FusedIterator for Iter<'_> {}

impl Type {
    /// Returns true if `self` is a `string` or a string literal.
    pub fn is_str(&self) -> bool {
        match self {
            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsStringKeyword,
                ..
            })
            | Type::Lit(LitType {
                lit: RTsLit::Str(..), ..
            }) => true,
            _ => false,
        }
    }

    pub fn is_str_lit(&self) -> bool {
        match self {
            Type::Lit(LitType {
                lit: RTsLit::Str(..), ..
            }) => true,
            _ => false,
        }
    }

    pub fn is_num(&self) -> bool {
        match self {
            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsNumberKeyword,
                ..
            })
            | Type::Lit(LitType {
                lit: RTsLit::Number(..),
                ..
            }) => true,
            _ => false,
        }
    }

    pub fn is_num_lit(&self) -> bool {
        match self {
            Type::Lit(LitType {
                lit: RTsLit::Number(..),
                ..
            }) => true,
            _ => false,
        }
    }

    /// Returns true if `self` is a `boolean` or a boolean literal.
    pub fn is_bool(&self) -> bool {
        match self {
            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsBooleanKeyword,
                ..
            })
            | Type::Lit(LitType {
                lit: RTsLit::Bool(..), ..
            }) => true,
            _ => false,
        }
    }
}

impl Type {
    /// Evaluates the type as a bool.
    pub fn as_bool(&self) -> Value<bool> {
        match self {
            Type::Class(_) | Type::TypeLit(_) => Known(true),

            Type::Lit(ty) => Known(match &ty.lit {
                RTsLit::Number(v) => v.value != 0.0,
                RTsLit::Str(v) => v.value != *"",
                RTsLit::Tpl(v) => v.quasis.first().unwrap().raw.value != *"",
                RTsLit::Bool(v) => v.value,
                RTsLit::BigInt(v) => v.value != BigInt::zero(),
            }),
            Type::Keyword(KeywordType { kind, .. }) => Known(match kind {
                TsKeywordTypeKind::TsNeverKeyword
                | TsKeywordTypeKind::TsStringKeyword
                | TsKeywordTypeKind::TsNumberKeyword
                | TsKeywordTypeKind::TsUnknownKeyword
                | TsKeywordTypeKind::TsBooleanKeyword
                | TsKeywordTypeKind::TsAnyKeyword
                | TsKeywordTypeKind::TsIntrinsicKeyword => return Unknown,
                TsKeywordTypeKind::TsSymbolKeyword
                | TsKeywordTypeKind::TsBigIntKeyword
                | TsKeywordTypeKind::TsObjectKeyword => true,

                TsKeywordTypeKind::TsUndefinedKeyword
                | TsKeywordTypeKind::TsNullKeyword
                | TsKeywordTypeKind::TsVoidKeyword => false,
            }),

            _ => Unknown,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct StaticThis {
    pub span: Span,
    pub metadata: StaticThisMetadata,
}

assert_eq_size!(StaticThis, [u8; 24]);

#[derive(Debug, Clone, PartialEq, Eq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct ThisType {
    pub span: Span,
    pub metadata: ThisTypeMetadata,
}

assert_eq_size!(ThisType, [u8; 24]);

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct TplType {
    pub span: Span,

    #[use_eq_ignore_span]
    pub quasis: Vec<RTplElement>,
    pub types: Vec<BoxedArcCow<Type>>,

    pub metadata: TplTypeMetadata,
}

assert_eq_size!(TplType, [u8; 72]);

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
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Visit)]
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
    #[instrument(skip(self))]
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

impl<T> Visit<Arc<T>> for ValidityChecker
where
    Arc<T>: Visitable,
{
    fn visit(&mut self, _: &Arc<T>) {}
}

impl Visit<Type> for ValidityChecker {
    fn visit(&mut self, ty: &Type) {
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

        if ty.types.iter().any(|t| t.is_intersection_type()) {
            self.valid = false;
            return;
        }

        ty.visit_children_with(self);
    }
}

macro_rules! impl_freezed {
    ($ty:ty) => {
        impl Freeze for $ty {
            fn freezed(self) -> Self {
                self.fold_with(&mut stc_arc_cow::freeze::Freezer)
            }
        }
    };
}

impl_freezed!(Mapped);
impl_freezed!(TsExpr);
