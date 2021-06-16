//! This crate exists to reduce compile time.
//!
//! The visitor is too slow to compile everytime I make change.
#![deny(unused)]
#![allow(incomplete_features)]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(specialization)]

pub use self::convert::rprop_name_to_expr;
pub use self::metadata::TypeElMetadata;
pub use self::metadata::TypeLitMetadata;
pub use self::symbol::SymbolId;
pub use self::{id::Id, module_id::ModuleId};
use fxhash::FxHashMap;
use is_macro::Is;
use num_bigint::BigInt;
use num_traits::Zero;
use rnode::FoldWith;
use rnode::NodeId;
use rnode::VisitMut;
use rnode::VisitMutWith;
use rnode::VisitWith;
use static_assertions::assert_eq_size;
use stc_ts_ast_rnode::RBigInt;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RNumber;
use stc_ts_ast_rnode::RPat;
use stc_ts_ast_rnode::RPrivateName;
use stc_ts_ast_rnode::RStr;
use stc_ts_ast_rnode::RTplElement;
use stc_ts_ast_rnode::RTsEntityName;
use stc_ts_ast_rnode::RTsEnumMemberId;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_ast_rnode::RTsLit;
use stc_ts_ast_rnode::RTsLitType;
use stc_ts_ast_rnode::RTsModuleName;
use stc_ts_ast_rnode::RTsNamespaceDecl;
use stc_ts_ast_rnode::RTsThisType;
use stc_ts_ast_rnode::RTsThisTypeOrIdent;
use stc_utils::error::context;
use stc_visit::Visit;
use stc_visit::Visitable;
use std::borrow::Cow;
use std::fmt;
use std::fmt::Formatter;
use std::{
    fmt::Debug,
    iter::FusedIterator,
    mem::{replace, transmute},
    ops::AddAssign,
    sync::Arc,
};
use swc_atoms::js_word;
use swc_atoms::JsWord;
use swc_common::EqIgnoreSpan;
use swc_common::TypeEq;
use swc_common::{FromVariant, Span, Spanned, DUMMY_SP};
use swc_ecma_ast::{Accessibility, MethodKind, TruePlusMinus, TsKeywordTypeKind, TsTypeOperatorOp};
use swc_ecma_utils::{
    Value,
    Value::{Known, Unknown},
};

mod convert;
mod id;
pub mod macros;
mod metadata;
pub mod module_id;
pub mod name;
mod symbol;

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
#[derive(Debug, Clone, PartialEq, Spanned, FromVariant, Is, EqIgnoreSpan, Visit)]
pub enum Type {
    Instance(Instance),
    StaticThis(StaticThis),
    This(RTsThisType),
    Lit(RTsLitType),
    Query(QueryType),
    Infer(InferType),
    Import(ImportType),
    Predicate(Predicate),
    IndexedAccessType(IndexedAccessType),

    #[is(name = "ref_type")]
    Ref(Ref),
    TypeLit(TypeLit),
    Keyword(RTsKeywordType),
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

    Arc(Freezed),

    Rest(RestType),

    Optional(OptionalType),

    Symbol(Symbol),

    Tpl(TplType),
}

assert_eq_size!(Type, [u8; 128]);

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
            (Type::Operator(l), Type::Operator(r)) => l.type_eq(r),
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
            _ => false,
        }
    }
}

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
            Key::Normal { span, sym } => Cow::Owned(Type::Lit(RTsLitType {
                node_id: NodeId::invalid(),
                span: *span,
                lit: RTsLit::Str(RStr {
                    span: *span,
                    value: sym.clone(),
                    has_escape: false,
                    kind: Default::default(),
                }),
            })),
            Key::Num(n) => Cow::Owned(Type::Lit(RTsLitType {
                node_id: NodeId::invalid(),
                span: n.span,
                lit: RTsLit::Number(n.clone()),
            })),
            Key::BigInt(n) => Cow::Owned(Type::Lit(RTsLitType {
                node_id: NodeId::invalid(),
                span: n.span,
                lit: RTsLit::BigInt(n.clone()),
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
    pub ty: Box<Type>,
}

assert_eq_size!(ComputedKey, [u8; 32]);

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
    pub ty: Box<Type>,
}

assert_eq_size!(Instance, [u8; 24]);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Symbol {
    pub span: Span,
    pub id: SymbolId,
}

assert_eq_size!(Symbol, [u8; 24]);

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct RestType {
    pub span: Span,
    pub ty: Box<Type>,
}

assert_eq_size!(RestType, [u8; 24]);

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct OptionalType {
    pub span: Span,
    pub ty: Box<Type>,
}

assert_eq_size!(OptionalType, [u8; 24]);

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct IndexedAccessType {
    pub span: Span,
    pub readonly: bool,
    pub obj_type: Box<Type>,
    pub index_type: Box<Type>,
}

assert_eq_size!(IndexedAccessType, [u8; 32]);

#[derive(Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Ref {
    pub span: Span,
    /// Id of the module where the ref is used in.
    pub ctxt: ModuleId,
    #[use_eq_ignore_span]
    pub type_name: RTsEntityName,
    pub type_args: Option<Box<TypeParamInstantiation>>,
}

assert_eq_size!(Ref, [u8; 64]);

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
}

assert_eq_size!(InferType, [u8; 64]);

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct QueryType {
    pub span: Span,
    pub expr: Box<QueryExpr>,
}

assert_eq_size!(QueryType, [u8; 24]);

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
}

assert_eq_size!(ImportType, [u8; 88]);

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Module {
    pub span: Span,
    #[use_eq_ignore_span]
    pub name: RTsModuleName,
    pub exports: Box<ModuleTypeData>,
}

assert_eq_size!(Module, [u8; 64]);

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
}

assert_eq_size!(Enum, [u8; 72]);

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
}

assert_eq_size!(Class, [u8; 24]);

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct ClassDef {
    pub span: Span,
    pub is_abstract: bool,
    pub name: Option<Id>,
    pub super_class: Option<Box<Type>>,
    pub body: Vec<ClassMember>,
    pub type_params: Option<TypeParamDecl>,
    pub implements: Box<Vec<TsExpr>>,
}

assert_eq_size!(ClassDef, [u8; 112]);

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
    pub ret_ty: Box<Type>,
    #[use_eq]
    pub kind: MethodKind,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct ClassProperty {
    pub span: Span,
    #[use_eq_ignore_span]
    pub key: Key,
    pub value: Option<Box<Type>>,
    pub is_static: bool,
    #[use_eq]
    pub accessibility: Option<Accessibility>,
    pub is_abstract: bool,
    pub is_optional: bool,
    pub readonly: bool,
    pub definite: bool,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Mapped {
    pub span: Span,
    #[use_eq]
    pub readonly: Option<TruePlusMinus>,
    #[use_eq]
    pub optional: Option<TruePlusMinus>,
    pub name_type: Option<Box<Type>>,
    pub type_param: TypeParam,
    pub ty: Option<Box<Type>>,
}

assert_eq_size!(Mapped, [u8; 80]);

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Conditional {
    pub span: Span,
    pub check_type: Box<Type>,
    pub extends_type: Box<Type>,
    pub true_type: Box<Type>,
    pub false_type: Box<Type>,
}

assert_eq_size!(Conditional, [u8; 48]);

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Operator {
    pub span: Span,
    #[use_eq]
    pub op: TsTypeOperatorOp,
    pub ty: Box<Type>,
}

assert_eq_size!(Operator, [u8; 24]);

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Tuple {
    pub span: Span,
    pub elems: Vec<TupleElement>,
}

assert_eq_size!(Tuple, [u8; 40]);

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct TupleElement {
    pub span: Span,
    #[not_type]
    pub label: Option<RPat>,
    pub ty: Box<Type>,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Alias {
    pub span: Span,
    pub type_params: Option<TypeParamDecl>,
    pub ty: Box<Type>,
}

assert_eq_size!(Alias, [u8; 64]);

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Interface {
    pub span: Span,
    pub name: Id,
    pub type_params: Option<TypeParamDecl>,
    pub extends: Vec<TsExpr>,
    pub body: Vec<TypeElement>,
}

assert_eq_size!(Interface, [u8; 120]);

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct TypeLit {
    pub span: Span,
    pub members: Vec<TypeElement>,
    pub metadata: TypeLitMetadata,
}

assert_eq_size!(TypeLit, [u8; 40]);

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

    /// TODO: Rename to `args`.
    pub params: Vec<Type>,
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
    pub ret_ty: Option<Box<Type>>,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct ConstructorSignature {
    pub span: Span,
    /// Only for synthesized type elements.
    #[use_eq]
    pub accessibility: Option<Accessibility>,
    pub params: Vec<FnParam>,
    pub ret_ty: Option<Box<Type>>,
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
    pub type_ann: Option<Box<Type>>,
    pub type_params: Option<TypeParamDecl>,
    pub metadata: TypeElMetadata,
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
    pub ret_ty: Option<Box<Type>>,
    pub type_params: Option<TypeParamDecl>,
    pub metadata: TypeElMetadata,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct IndexSignature {
    pub params: Vec<FnParam>,
    pub type_ann: Option<Box<Type>>,

    pub readonly: bool,
    pub span: Span,

    pub is_static: bool,
}

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Array {
    pub span: Span,
    pub elem_type: Box<Type>,
}

assert_eq_size!(Array, [u8; 24]);

/// a | b
#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Union {
    pub span: Span,
    pub types: Vec<Type>,
}

assert_eq_size!(Union, [u8; 40]);

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
                    panic!("[INVALID_TYPE]: A union type has duplicate elements: ({:?})", t1)
                }
            }
        }

        if self.types.len() <= 1 {
            panic!(
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
    pub ty: Box<Type>,
}

/// a & b
#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Intersection {
    pub span: Span,
    pub types: Vec<Type>,
}

assert_eq_size!(Intersection, [u8; 40]);

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
                    panic!(
                        "[INVALID_TYPE]: An intersection type has duplicate elements: ({:?})",
                        t1
                    )
                }
            }
        }

        if self.types.len() <= 1 {
            panic!(
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
    pub constraint: Option<Box<Type>>,
    pub default: Option<Box<Type>>,
}

/// FooEnum.A
#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct EnumVariant {
    pub span: Span,
    pub ctxt: ModuleId,
    pub enum_name: Id,
    /// [None] if for the general instance type of an enum.
    pub name: Option<JsWord>,
}

assert_eq_size!(EnumVariant, [u8; 40]);

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Function {
    pub span: Span,
    pub type_params: Option<TypeParamDecl>,
    pub params: Vec<FnParam>,
    pub ret_ty: Box<Type>,
}

assert_eq_size!(Function, [u8; 88]);

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Constructor {
    pub span: Span,
    pub type_params: Option<TypeParamDecl>,
    pub params: Vec<FnParam>,
    /// The return type.
    pub type_ann: Box<Type>,
    pub is_abstract: bool,
}

assert_eq_size!(Constructor, [u8; 88]);

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct Predicate {
    pub span: Span,
    #[use_eq_ignore_span]
    pub param_name: RTsThisTypeOrIdent,
    pub asserts: bool,
    pub ty: Option<Box<Type>>,
}

assert_eq_size!(Predicate, [u8; 64]);

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct TypeOrSpread {
    pub span: Span,
    pub spread: Option<Span>,
    pub ty: Box<Type>,
}

pub trait TypeIterExt {}

impl Type {
    pub fn intersection<I>(span: Span, iter: I) -> Self
    where
        I: IntoIterator<Item = Type>,
    {
        let mut tys = vec![];

        for ty in iter {
            match ty {
                Type::Intersection(Intersection { types, .. }) => {
                    tys.extend(types);
                }

                _ => tys.push(ty),
            }
        }

        let has_str = tys.iter().any(|ty| ty.is_str());
        // TODO
        let has_bool = tys.iter().any(|ty| ty.is_kwd(TsKeywordTypeKind::TsBooleanKeyword));
        let has_num = tys.iter().any(|ty| ty.is_num());

        if (has_str && has_bool) || (has_bool && has_num) || (has_num && has_str) {
            return Type::never(span);
        }

        if tys.iter().any(|ty| ty.is_never()) {
            return Type::never(span);
        }

        match tys.len() {
            0 => Type::never(span),
            1 => tys.into_iter().next().unwrap(),
            _ => Type::Intersection(Intersection { span, types: tys }),
        }
    }

    /// Creates a new type from `iter`.
    ///
    /// Note:
    ///
    ///  - never types are excluded.
    pub fn union<I: IntoIterator<Item = Self> + Debug>(iter: I) -> Self {
        let _ctx = context(format!("Iterator: {:?}", iter));

        let mut span = DUMMY_SP;

        let mut elements = vec![];

        for ty in iter {
            let sp = ty.span();

            if sp.lo() < span.lo() {
                span = span.with_lo(sp.lo());
            }
            if sp.hi() > span.hi() {
                span = span.with_hi(sp.hi());
            }

            if ty.normalize().is_union_type() {
                let types = ty.foldable().union_type().unwrap().types;
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

        let ty = match elements.len() {
            0 => Type::never(span),
            1 => elements.into_iter().next().unwrap(),
            _ => Type::Union(Union { span, types: elements }),
        };
        ty.assert_valid();
        ty
    }

    /// If `self` is [Type::Lit], convert it to [Type::Keyword].
    pub fn force_generalize_top_level_literals(self) -> Self {
        match self {
            Type::Lit(lit) => Type::Keyword(RTsKeywordType {
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
            }),
            _ => self,
        }
    }

    pub fn contains_void(&self) -> bool {
        match self.normalize() {
            Type::Instance(ty) => ty.ty.contains_void(),

            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsVoidKeyword,
                ..
            }) => true,

            Type::Union(ref t) => t.types.iter().any(|t| t.contains_void()),

            _ => false,
        }
    }

    pub fn is_any(&self) -> bool {
        match self.normalize() {
            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsAnyKeyword,
                ..
            }) => true,

            Type::Union(t) => t.types.iter().any(|t| t.is_any()),

            Type::Instance(ty) => ty.ty.is_any(),

            _ => false,
        }
    }

    pub fn is_unknown(&self) -> bool {
        match *self.normalize() {
            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsUnknownKeyword,
                ..
            }) => true,

            Type::Union(ref t) => t.types.iter().any(|t| t.is_unknown()),

            _ => false,
        }
    }

    pub fn contains_undefined(&self) -> bool {
        match *self.normalize() {
            Type::Keyword(RTsKeywordType {
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
        if !cfg!(debug_assertions) {
            return true;
        }

        match self {
            Type::Arc(..) | Type::Keyword(..) | Type::This(..) | Type::StaticThis(..) | Type::Symbol(..) => true,

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
        match self.normalize() {
            Type::Instance(ty) => ty.ty.is_kwd(k),
            Type::Keyword(RTsKeywordType { kind, .. }) if *kind == k => true,
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

    pub fn never<'any>(span: Span) -> Self {
        Type::Keyword(RTsKeywordType {
            span,
            kind: TsKeywordTypeKind::TsNeverKeyword,
        })
    }

    pub fn undefined<'any>(span: Span) -> Self {
        Type::Keyword(RTsKeywordType {
            span,
            kind: TsKeywordTypeKind::TsUndefinedKeyword,
        })
    }

    pub fn any<'any>(span: Span) -> Self {
        Type::Keyword(RTsKeywordType {
            span,
            kind: TsKeywordTypeKind::TsAnyKeyword,
        })
    }

    pub fn void<'any>(span: Span) -> Self {
        Type::Keyword(RTsKeywordType {
            span,
            kind: TsKeywordTypeKind::TsVoidKeyword,
        })
    }

    pub fn unknown<'any>(span: Span) -> Self {
        Type::Keyword(RTsKeywordType {
            span,
            kind: TsKeywordTypeKind::TsUnknownKeyword,
        })
    }
}

impl Type {
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

            Type::Arc(ty) => ty.span = span,

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
        }
    }
}

struct AssertValid;

impl Visit<Union> for AssertValid {
    fn visit(&mut self, ty: &Union) {
        if !cfg!(debug_assertions) {
            return;
        }

        ty.visit_children_with(self);

        ty.assert_valid();

        for item in ty.types.iter() {
            if item.normalize().is_union_type() {
                panic!("[INVALID_TYPE]: A union type should not have a union item")
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
            if item.normalize().is_intersection_type() {
                panic!("[INVALID_TYPE]: An intersection type should not have an intersection item")
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

        let _ctx = context(format!("{:?}", self));

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
    /// TODO: Remove if possible
    pub fn foldable(mut self) -> Type {
        self.normalize_mut();
        self
    }

    /// `Type::Static` is normalized.
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

    /// `Type::Static` is normalized.
    ///
    /// TODO: Remove if possible
    pub fn normalize_mut(&mut self) -> &mut Type {
        match self {
            Type::Arc(Freezed { ty, span }) => {
                let mut ty = (**ty).clone();
                ty.respan(*span);
                *self = ty;
            }
            _ => {}
        }

        self
    }

    /// TODO: Make this more efficient, and explode subunions.
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
        match self.ty.normalize() {
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
        match self.normalize() {
            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsStringKeyword,
                ..
            })
            | Type::Lit(RTsLitType {
                lit: RTsLit::Str(..), ..
            }) => true,
            _ => false,
        }
    }

    pub fn is_str_lit(&self) -> bool {
        match self.normalize() {
            Type::Lit(RTsLitType {
                lit: RTsLit::Str(..), ..
            }) => true,
            _ => false,
        }
    }

    pub fn is_num(&self) -> bool {
        match self.normalize() {
            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsNumberKeyword,
                ..
            })
            | Type::Lit(RTsLitType {
                lit: RTsLit::Number(..),
                ..
            }) => true,
            _ => false,
        }
    }

    pub fn is_num_lit(&self) -> bool {
        match self.normalize() {
            Type::Lit(RTsLitType {
                lit: RTsLit::Number(..),
                ..
            }) => true,
            _ => false,
        }
    }

    /// Returns true if `self` is a `boolean` or a boolean literal.
    pub fn is_bool(&self) -> bool {
        match self.normalize() {
            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsBooleanKeyword,
                ..
            })
            | Type::Lit(RTsLitType {
                lit: RTsLit::Bool(..), ..
            }) => true,
            _ => false,
        }
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

struct CheapClone;

impl VisitMut<Type> for CheapClone {
    fn visit_mut(&mut self, ty: &mut Type) {
        ty.visit_mut_children_with(self);

        if ty.is_clone_cheap() {
            return;
        }

        let new_ty = replace(
            ty,
            Type::Keyword(RTsKeywordType {
                span: DUMMY_SP,
                kind: TsKeywordTypeKind::TsAnyKeyword,
            }),
        );

        *ty = Type::Arc(Freezed {
            span: new_ty.span(),
            ty: Arc::new(new_ty),
        })
    }
}

impl Type {
    /// Make cloning cheap.
    #[inline]
    pub fn cheap(mut self) -> Self {
        self.make_cheap();
        self
    }

    /// Make cloning cheap.
    #[inline]
    pub fn make_cheap(&mut self) {
        self.visit_mut_with(&mut CheapClone);
    }

    pub fn as_bool(&self) -> Value<bool> {
        match self {
            Type::Arc(ref ty) => ty.ty.as_bool(),

            Type::Class(_) | Type::TypeLit(_) => Known(true),

            Type::Lit(ty) => Known(match &ty.lit {
                RTsLit::Number(v) => v.value != 0.0,
                RTsLit::Str(v) => v.value != *"",
                RTsLit::Tpl(v) => v.quasis.first().unwrap().raw.value != *"",
                RTsLit::Bool(v) => v.value,
                RTsLit::BigInt(v) => v.value != BigInt::zero(),
            }),
            Type::Keyword(RTsKeywordType { kind, .. }) => Known(match kind {
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
}

assert_eq_size!(StaticThis, [u8; 12]);

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq, Visit)]
pub struct TplType {
    pub span: Span,

    #[use_eq_ignore_span]
    pub quasis: Vec<RTplElement>,
    pub types: Vec<Type>,
}

assert_eq_size!(TplType, [u8; 64]);

#[derive(Debug, Clone, PartialEq, Spanned, EqIgnoreSpan, TypeEq)]
pub struct Freezed {
    pub span: Span,
    pub ty: Arc<Type>,
}

assert_eq_size!(Freezed, [u8; 24]);

impl Visitable for Freezed {}

impl<V> VisitWith<V> for Freezed
where
    V: ?Sized,
{
    fn visit_children_with(&self, visitor: &mut V) {
        self.span.visit_with(visitor);
        self.ty.visit_with(visitor);
    }
}

impl<V> VisitMutWith<V> for Freezed
where
    V: ?Sized,
{
    fn visit_mut_children_with(&mut self, v: &mut V) {
        self.span.visit_mut_with(v);
    }
}

impl<V> FoldWith<V> for Freezed
where
    V: ?Sized,
{
    fn fold_children_with(mut self, v: &mut V) -> Self {
        self.span = self.span.fold_with(v);
        self
    }
}
