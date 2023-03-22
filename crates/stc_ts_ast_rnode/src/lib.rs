#![feature(box_syntax)]
#![allow(clippy::derive_partial_eq_without_eq)]

use rnode::{define_rnode, NodeId};
use swc_atoms::{Atom, JsWord};
use swc_common::{EqIgnoreSpan, Span, Spanned, TypeEq};
use swc_ecma_ast::*;

type BigIntValue = Box<num_bigint::BigInt>;

impl RIdent {
    pub const fn new(sym: JsWord, span: Span) -> Self {
        Self {
            sym,
            span,
            optional: false,
            node_id: NodeId::invalid(),
        }
    }
}

impl RExpr {
    pub fn is_new_target(&self) -> bool {
        matches!(
            self,
            RExpr::MetaProp(RMetaPropExpr {
                kind: MetaPropKind::NewTarget,
                ..
            })
        )
    }

    pub fn is_arrow_expr(&self) -> bool {
        match self {
            RExpr::Arrow(RArrowExpr { .. }) => true,
            RExpr::Paren(RParenExpr { expr, .. }) => expr.is_arrow_expr(),
            _ => false,
        }
    }
}

impl From<RTsEntityName> for RExpr {
    fn from(v: RTsEntityName) -> Self {
        match v {
            RTsEntityName::Ident(v) => RExpr::Ident(v),
            RTsEntityName::TsQualifiedName(v) => RExpr::Member(RMemberExpr {
                node_id: NodeId::invalid(),
                span: v.span(),
                obj: box v.left.into(),
                prop: RMemberProp::Ident(v.right),
            }),
        }
    }
}

impl From<Box<RExpr>> for RTsEntityName {
    fn from(v: Box<RExpr>) -> Self {
        match *v {
            RExpr::Ident(v) => RTsEntityName::Ident(v),
            RExpr::Member(RMemberExpr { obj, prop, .. }) => RTsEntityName::TsQualifiedName(box RTsQualifiedName {
                node_id: NodeId::invalid(),
                left: obj.into(),
                right: match prop {
                    RMemberProp::Ident(v) => v,
                    _ => unreachable!("invalid member expression"),
                },
            }),
            _ => unreachable!("invalid expression"),
        }
    }
}

impl From<Atom> for RStr {
    fn from(v: Atom) -> Self {
        Self {
            value: (&*v).into(),
            span: Default::default(),
            raw: None,
        }
    }
}

impl From<Atom> for RLit {
    fn from(v: Atom) -> Self {
        Self::Str(v.into())
    }
}

impl From<Atom> for RTsLit {
    fn from(v: Atom) -> Self {
        Self::Str(v.into())
    }
}

/// Impl `TypeEq` using `EqIgnoreSpan`
macro_rules! type_eq {
    ($T:ty) => {
        impl TypeEq for $T {
            #[inline]
            fn type_eq(&self, other: &Self) -> bool {
                self.eq_ignore_span(&other)
            }
        }
    };
}

type_eq!(RTsKeywordType);
type_eq!(RTsThisType);
type_eq!(RTsThisTypeOrIdent);
type_eq!(RIdent);
type_eq!(RTsEntityName);
type_eq!(RTsNamespaceDecl);
type_eq!(RTsEnumMemberId);
type_eq!(RExpr);
type_eq!(RPropName);

impl TypeEq for RBool {
    #[inline]
    fn type_eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl TypeEq for RStr {
    #[inline]
    fn type_eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl TypeEq for RNumber {
    #[inline]
    fn type_eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl TypeEq for RBigInt {
    #[inline]
    fn type_eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl TypeEq for RTsLitType {
    fn type_eq(&self, other: &Self) -> bool {
        self.lit.type_eq(&other.lit)
    }
}

impl TypeEq for RTsLit {
    fn type_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (RTsLit::Str(a), RTsLit::Str(b)) => a.type_eq(b),
            (RTsLit::Number(a), RTsLit::Number(b)) => a.type_eq(b),
            (RTsLit::Bool(a), RTsLit::Bool(b)) => a.type_eq(b),
            (RTsLit::BigInt(a), RTsLit::BigInt(b)) => a.type_eq(b),
            _ => false,
        }
    }
}

impl RExpr {
    pub fn node_id(&self) -> Option<NodeId> {
        match self {
            RExpr::This(v) => Some(v.node_id),
            RExpr::Array(v) => Some(v.node_id),
            RExpr::Object(v) => Some(v.node_id),
            RExpr::Fn(v) => Some(v.node_id),
            RExpr::Unary(v) => Some(v.node_id),
            RExpr::Update(v) => Some(v.node_id),
            RExpr::Bin(v) => Some(v.node_id),
            RExpr::Assign(v) => Some(v.node_id),
            RExpr::Member(v) => Some(v.node_id),
            RExpr::SuperProp(v) => Some(v.node_id),
            RExpr::Cond(v) => Some(v.node_id),
            RExpr::Call(v) => Some(v.node_id),
            RExpr::New(v) => Some(v.node_id),
            RExpr::Seq(v) => Some(v.node_id),
            RExpr::Ident(v) => Some(v.node_id),
            RExpr::Lit(..) => None,
            RExpr::Tpl(v) => Some(v.node_id),
            RExpr::TaggedTpl(v) => Some(v.node_id),
            RExpr::Arrow(v) => Some(v.node_id),
            RExpr::Class(v) => Some(v.node_id),
            RExpr::Yield(v) => Some(v.node_id),
            RExpr::MetaProp(v) => Some(v.node_id),
            RExpr::Await(v) => Some(v.node_id),
            RExpr::Paren(v) => Some(v.node_id),
            RExpr::JSXMember(v) => Some(v.node_id),
            RExpr::JSXNamespacedName(v) => Some(v.node_id),
            RExpr::JSXEmpty(v) => Some(v.node_id),
            RExpr::JSXElement(v) => Some(v.node_id),
            RExpr::JSXFragment(v) => Some(v.node_id),
            RExpr::TsTypeAssertion(v) => Some(v.node_id),
            RExpr::TsConstAssertion(v) => Some(v.node_id),
            RExpr::TsNonNull(v) => Some(v.node_id),
            RExpr::TsAs(v) => Some(v.node_id),
            RExpr::TsInstantiation(v) => Some(v.node_id),
            RExpr::TsSatisfies(v) => Some(v.node_id),
            RExpr::PrivateName(v) => Some(v.node_id),
            RExpr::OptChain(v) => Some(v.node_id),
            RExpr::Invalid(..) => None,
        }
    }
}

define_rnode!({
    pub struct Class {
        pub span: Span,
        pub decorators: Vec<Decorator>,
        pub body: Vec<ClassMember>,
        pub super_class: Option<Box<Expr>>,
        pub is_abstract: bool,
        pub type_params: Option<Box<TsTypeParamDecl>>,
        pub super_type_params: Option<Box<TsTypeParamInstantiation>>,
        pub implements: Vec<TsExprWithTypeArgs>,
    }

    pub enum ClassMember {
        Constructor(Constructor),
        Method(ClassMethod),
        PrivateMethod(PrivateMethod),
        ClassProp(ClassProp),
        PrivateProp(PrivateProp),
        TsIndexSignature(TsIndexSignature),
        StaticBlock(StaticBlock),
        AutoAccessor(AutoAccessor),
        Empty(EmptyStmt),
    }

    pub struct ClassProp {
        pub span: Span,
        pub key: PropName,
        pub value: Option<Box<Expr>>,
        pub type_ann: Option<Box<TsTypeAnn>>,
        pub is_static: bool,
        pub decorators: Vec<Decorator>,
        #[not_spanned]
        pub accessibility: Option<Accessibility>,
        pub is_abstract: bool,
        pub is_optional: bool,
        pub readonly: bool,
        pub declare: bool,
        pub definite: bool,
        pub is_override: bool,
    }
    pub struct PrivateProp {
        pub span: Span,
        pub key: PrivateName,
        pub value: Option<Box<Expr>>,
        pub type_ann: Option<Box<TsTypeAnn>>,
        pub is_static: bool,
        pub decorators: Vec<Decorator>,
        #[not_spanned]
        pub accessibility: Option<Accessibility>,
        pub is_optional: bool,
        pub readonly: bool,
        pub definite: bool,
        pub is_override: bool,
    }
    pub struct ClassMethod {
        pub span: Span,
        pub key: PropName,
        pub function: Box<Function>,
        #[not_spanned]
        pub kind: MethodKind,
        pub is_static: bool,
        #[not_spanned]
        pub accessibility: Option<Accessibility>,
        pub is_abstract: bool,
        pub is_optional: bool,
        pub is_override: bool,
    }
    pub struct PrivateMethod {
        pub span: Span,
        pub key: PrivateName,
        pub function: Box<Function>,
        #[not_spanned]
        pub kind: MethodKind,
        pub is_static: bool,
        #[not_spanned]
        pub accessibility: Option<Accessibility>,
        pub is_abstract: bool,
        pub is_optional: bool,
        pub is_override: bool,
    }
    pub struct Constructor {
        pub span: Span,
        pub key: PropName,
        pub params: Vec<ParamOrTsParamProp>,
        pub body: Option<BlockStmt>,
        #[not_spanned]
        pub accessibility: Option<Accessibility>,
        pub is_optional: bool,
    }
    pub struct Decorator {
        pub span: Span,
        pub expr: Box<Expr>,
    }

    pub enum Decl {
        Class(ClassDecl),
        Fn(FnDecl),
        Var(Box<VarDecl>),
        TsInterface(Box<TsInterfaceDecl>),
        TsTypeAlias(Box<TsTypeAliasDecl>),
        TsEnum(Box<TsEnumDecl>),
        TsModule(Box<TsModuleDecl>),
    }
    pub struct FnDecl {
        pub ident: Ident,
        pub declare: bool,
        #[span]
        pub function: Box<Function>,
    }
    pub struct ClassDecl {
        pub ident: Ident,
        pub declare: bool,
        #[span]
        pub class: Box<Class>,
    }
    pub struct VarDecl {
        pub span: Span,
        #[not_spanned]
        pub kind: VarDeclKind,
        pub declare: bool,
        pub decls: Vec<VarDeclarator>,
    }

    pub struct VarDeclarator {
        pub span: Span,
        pub name: Pat,
        pub init: Option<Box<Expr>>,
        pub definite: bool,
    }
    pub enum Expr {
        This(ThisExpr),
        Array(ArrayLit),
        Object(ObjectLit),
        Fn(FnExpr),
        Unary(UnaryExpr),
        Update(UpdateExpr),
        Bin(BinExpr),
        Assign(AssignExpr),
        Member(MemberExpr),
        SuperProp(SuperPropExpr),
        Cond(CondExpr),
        Call(CallExpr),
        New(NewExpr),
        Seq(SeqExpr),
        Ident(Ident),
        Lit(Lit),
        Tpl(Tpl),
        TaggedTpl(TaggedTpl),
        Arrow(ArrowExpr),
        Class(ClassExpr),
        Yield(YieldExpr),
        MetaProp(MetaPropExpr),
        Await(AwaitExpr),
        Paren(ParenExpr),
        JSXMember(JSXMemberExpr),
        JSXNamespacedName(JSXNamespacedName),
        JSXEmpty(JSXEmptyExpr),
        JSXElement(Box<JSXElement>),
        JSXFragment(JSXFragment),
        TsTypeAssertion(TsTypeAssertion),
        TsConstAssertion(TsConstAssertion),
        TsNonNull(TsNonNullExpr),
        TsAs(TsAsExpr),
        TsInstantiation(TsInstantiation),
        TsSatisfies(TsSatisfiesExpr),
        PrivateName(PrivateName),
        OptChain(OptChainExpr),
        Invalid(Invalid),
    }
    pub struct ThisExpr {
        pub span: Span,
    }
    pub struct ArrayLit {
        pub span: Span,
        pub elems: Vec<Option<ExprOrSpread>>,
    }
    pub struct ObjectLit {
        pub span: Span,
        pub props: Vec<PropOrSpread>,
    }
    pub enum PropOrSpread {
        Spread(SpreadElement),
        Prop(Box<Prop>),
    }
    pub struct SpreadElement {
        pub dot3_token: Span,
        #[span]
        pub expr: Box<Expr>,
    }
    pub struct UnaryExpr {
        pub span: Span,
        #[not_spanned]
        pub op: UnaryOp,
        pub arg: Box<Expr>,
    }
    pub struct UpdateExpr {
        pub span: Span,
        #[not_spanned]
        pub op: UpdateOp,
        pub prefix: bool,
        pub arg: Box<Expr>,
    }
    pub struct BinExpr {
        pub span: Span,
        #[not_spanned]
        pub op: BinaryOp,
        pub left: Box<Expr>,
        pub right: Box<Expr>,
    }
    pub struct FnExpr {
        pub ident: Option<Ident>,
        #[span]
        pub function: Box<Function>,
    }
    pub struct ClassExpr {
        pub ident: Option<Ident>,
        #[span]
        pub class: Box<Class>,
    }
    pub struct AssignExpr {
        pub span: Span,
        #[not_spanned]
        pub op: AssignOp,
        pub left: PatOrExpr,
        pub right: Box<Expr>,
    }
    pub struct MemberExpr {
        pub span: Span,
        pub obj: Box<Expr>,
        pub prop: MemberProp,
    }
    pub enum MemberProp {
        Ident(Ident),
        PrivateName(PrivateName),
        Computed(ComputedPropName),
    }
    pub struct CondExpr {
        pub span: Span,
        pub test: Box<Expr>,
        pub cons: Box<Expr>,
        pub alt: Box<Expr>,
    }
    pub struct CallExpr {
        pub span: Span,
        pub callee: Callee,
        pub args: Vec<ExprOrSpread>,
        pub type_args: Option<Box<TsTypeParamInstantiation>>,
    }
    pub struct Super {
        pub span: Span,
    }
    pub struct Import {
        pub span: Span,
    }
    pub enum Callee {
        Expr(Box<Expr>),
        Super(Super),
        Import(Import),
    }
    pub struct NewExpr {
        pub span: Span,
        pub callee: Box<Expr>,
        pub args: Option<Vec<ExprOrSpread>>,
        pub type_args: Option<Box<TsTypeParamInstantiation>>,
    }
    pub struct SeqExpr {
        pub span: Span,
        pub exprs: Vec<Box<Expr>>,
    }
    pub struct ArrowExpr {
        pub span: Span,
        pub params: Vec<Pat>,
        pub body: Box<BlockStmtOrExpr>,
        pub is_async: bool,
        pub is_generator: bool,
        pub type_params: Option<Box<TsTypeParamDecl>>,
        pub return_type: Option<Box<TsTypeAnn>>,
    }
    pub struct YieldExpr {
        pub span: Span,
        pub arg: Option<Box<Expr>>,
        pub delegate: bool,
    }
    pub struct MetaPropExpr {
        pub span: Span,
        pub kind: MetaPropKind,
    }
    pub struct AwaitExpr {
        pub span: Span,
        pub arg: Box<Expr>,
    }
    pub struct Tpl {
        pub span: Span,
        pub exprs: Vec<Box<Expr>>,
        pub quasis: Vec<TplElement>,
    }
    pub struct TaggedTpl {
        pub span: Span,
        pub tag: Box<Expr>,
        pub tpl: Box<Tpl>,
        pub type_params: Option<Box<TsTypeParamInstantiation>>,
    }
    pub struct TplElement {
        pub span: Span,
        pub tail: bool,
        pub cooked: Option<Atom>,
        pub raw: Atom,
    }
    pub struct ParenExpr {
        pub span: Span,
        pub expr: Box<Expr>,
    }

    #[skip_node_id]
    pub struct ExprOrSpread {
        pub spread: Option<Span>,
        // TODO(kdy1): Use custom impl
        #[span]
        pub expr: Box<Expr>,
    }
    pub enum BlockStmtOrExpr {
        BlockStmt(BlockStmt),
        Expr(Box<Expr>),
    }
    pub enum PatOrExpr {
        Expr(Box<Expr>),
        Pat(Box<Pat>),
    }
    pub struct OptChainExpr {
        pub span: Span,
        pub question_dot_token: Span,
        pub base: Box<OptChainBase>,
    }
    pub enum OptChainBase {
        Member(MemberExpr),
        Call(OptCall),
    }
    pub struct OptCall {
        pub span: Span,
        pub callee: Box<Expr>,
        pub args: Vec<ExprOrSpread>,
        pub type_args: Option<Box<TsTypeParamInstantiation>>,
    }
    pub struct Function {
        pub params: Vec<Param>,
        pub decorators: Vec<Decorator>,
        pub span: Span,
        pub body: Option<BlockStmt>,
        pub is_generator: bool,
        pub is_async: bool,
        pub type_params: Option<Box<TsTypeParamDecl>>,
        pub return_type: Option<Box<TsTypeAnn>>,
    }
    pub struct Param {
        pub span: Span,
        pub decorators: Vec<Decorator>,
        pub pat: Pat,
    }
    pub enum ParamOrTsParamProp {
        TsParamProp(TsParamProp),
        Param(Param),
    }

    pub struct Ident {
        pub span: Span,
        pub sym: JsWord,
        pub optional: bool,
    }

    pub struct BindingIdent {
        #[span]
        pub id: Ident,
        pub type_ann: Option<Box<TsTypeAnn>>,
    }

    pub struct PrivateName {
        pub span: Span,
        pub id: Ident,
    }
    pub enum JSXObject {
        JSXMemberExpr(Box<JSXMemberExpr>),
        Ident(Ident),
    }
    pub struct JSXMemberExpr {
        #[span(lo)]
        pub obj: JSXObject,
        #[span(hi)]
        pub prop: Ident,
    }
    pub struct JSXNamespacedName {
        #[span(lo)]
        pub ns: Ident,
        #[span(hi)]
        pub name: Ident,
    }
    pub struct JSXEmptyExpr {
        pub span: Span,
    }
    pub struct JSXExprContainer {
        pub span: Span,
        pub expr: JSXExpr,
    }
    pub enum JSXExpr {
        JSXEmptyExpr(JSXEmptyExpr),
        Expr(Box<Expr>),
    }
    pub struct JSXSpreadChild {
        pub span: Span,
        pub expr: Box<Expr>,
    }
    pub enum JSXElementName {
        Ident(Ident),
        JSXMemberExpr(JSXMemberExpr),
        JSXNamespacedName(JSXNamespacedName),
    }
    pub struct JSXOpeningElement {
        pub name: JSXElementName,
        pub span: Span,
        pub attrs: Vec<JSXAttrOrSpread>,
        pub self_closing: bool,
        pub type_args: Option<Box<TsTypeParamInstantiation>>,
    }
    pub enum JSXAttrOrSpread {
        JSXAttr(JSXAttr),
        SpreadElement(SpreadElement),
    }
    pub struct JSXClosingElement {
        pub span: Span,
        pub name: JSXElementName,
    }
    pub struct JSXAttr {
        pub span: Span,
        pub name: JSXAttrName,
        pub value: Option<JSXAttrValue>,
    }
    pub enum JSXAttrName {
        Ident(Ident),
        JSXNamespacedName(JSXNamespacedName),
    }
    pub enum JSXAttrValue {
        Lit(Lit),
        JSXExprContainer(JSXExprContainer),
        JSXElement(Box<JSXElement>),
        JSXFragment(JSXFragment),
    }
    pub struct JSXText {
        pub span: Span,
        pub value: Atom,
        pub raw: Atom,
    }
    pub struct JSXElement {
        pub span: Span,
        pub opening: JSXOpeningElement,
        pub children: Vec<JSXElementChild>,
        pub closing: Option<JSXClosingElement>,
    }
    pub enum JSXElementChild {
        JSXText(JSXText),
        JSXExprContainer(JSXExprContainer),
        JSXSpreadChild(JSXSpreadChild),
        JSXElement(Box<JSXElement>),
        JSXFragment(JSXFragment),
    }
    pub struct JSXFragment {
        pub span: Span,
        pub opening: JSXOpeningFragment,
        pub children: Vec<JSXElementChild>,
        pub closing: JSXClosingFragment,
    }
    pub struct JSXOpeningFragment {
        pub span: Span,
    }
    #[skip_node_id]
    pub struct JSXClosingFragment {
        pub span: Span,
    }
    #[skip_node_id]
    pub struct Invalid {
        pub span: Span,
    }
    pub enum Lit {
        Str(Str),
        Bool(Bool),
        Null(Null),
        Num(Number),
        BigInt(BigInt),
        Regex(Regex),
        JSXText(JSXText),
    }
    #[skip_node_id]
    pub struct BigInt {
        pub span: Span,
        #[not_spanned]
        pub value: BigIntValue,

        pub raw: Option<Atom>,
    }
    #[skip_node_id]
    pub struct Str {
        pub span: Span,
        pub value: JsWord,

        pub raw: Option<Atom>,
    }
    #[skip_node_id]
    pub struct Bool {
        pub span: Span,
        pub value: bool,
    }
    #[skip_node_id]
    pub struct Null {
        pub span: Span,
    }
    #[skip_node_id]
    pub struct Regex {
        pub span: Span,
        pub exp: Atom,
        pub flags: Atom,
    }
    #[skip_node_id]
    pub struct Number {
        pub span: Span,
        pub value: f64,

        pub raw: Option<Atom>,
    }
    pub enum Program {
        Module(Module),
        Script(Script),
    }
    pub struct Module {
        pub span: Span,
        pub body: Vec<ModuleItem>,
        pub shebang: Option<Atom>,
    }
    pub struct Script {
        pub span: Span,
        pub body: Vec<Stmt>,
        pub shebang: Option<Atom>,
    }
    pub enum ModuleItem {
        ModuleDecl(ModuleDecl),
        Stmt(Stmt),
    }
    pub enum ModuleDecl {
        Import(ImportDecl),
        ExportDecl(ExportDecl),
        ExportNamed(NamedExport),
        ExportDefaultDecl(ExportDefaultDecl),
        ExportDefaultExpr(ExportDefaultExpr),
        ExportAll(ExportAll),
        TsImportEquals(Box<TsImportEqualsDecl>),
        TsExportAssignment(TsExportAssignment),
        TsNamespaceExport(TsNamespaceExportDecl),
    }
    pub struct ExportDefaultExpr {
        pub span: Span,
        pub expr: Box<Expr>,
    }
    pub struct ExportDecl {
        pub span: Span,
        pub decl: Decl,
    }
    pub struct ImportDecl {
        pub span: Span,
        pub specifiers: Vec<ImportSpecifier>,
        pub src: Box<Str>,
        pub type_only: bool,
        pub asserts: Option<Box<ObjectLit>>,
    }
    pub struct ExportAll {
        pub span: Span,
        pub src: Box<Str>,
        pub asserts: Option<Box<ObjectLit>>,
        pub type_only: bool,
    }
    pub struct NamedExport {
        pub span: Span,
        pub specifiers: Vec<ExportSpecifier>,
        pub src: Option<Box<Str>>,
        pub type_only: bool,
        pub asserts: Option<Box<ObjectLit>>,
    }
    pub struct ExportDefaultDecl {
        pub span: Span,
        pub decl: DefaultDecl,
    }
    pub enum DefaultDecl {
        Class(ClassExpr),
        Fn(FnExpr),
        TsInterfaceDecl(Box<TsInterfaceDecl>),
    }
    pub enum ImportSpecifier {
        Named(ImportNamedSpecifier),
        Default(ImportDefaultSpecifier),
        Namespace(ImportStarAsSpecifier),
    }
    pub struct ImportDefaultSpecifier {
        pub span: Span,
        pub local: Ident,
    }
    pub struct ImportStarAsSpecifier {
        pub span: Span,
        pub local: Ident,
    }
    pub struct ImportNamedSpecifier {
        pub span: Span,
        pub local: Ident,
        pub imported: Option<ModuleExportName>,
        pub is_type_only: bool,
    }
    pub enum ExportSpecifier {
        Namespace(ExportNamespaceSpecifier),
        Default(ExportDefaultSpecifier),
        Named(ExportNamedSpecifier),
    }
    pub struct ExportNamespaceSpecifier {
        pub span: Span,
        pub name: ModuleExportName,
    }
    pub struct ExportDefaultSpecifier {
        #[span]
        pub exported: Ident,
    }
    pub struct ExportNamedSpecifier {
        pub span: Span,
        pub orig: ModuleExportName,
        pub exported: Option<ModuleExportName>,
        pub is_type_only: bool,
    }
    pub enum ModuleExportName {
        Ident(Ident),
        Str(Str),
    }

    pub enum Pat {
        Ident(BindingIdent),
        Array(ArrayPat),
        Rest(RestPat),
        Object(ObjectPat),
        Assign(AssignPat),
        Invalid(Invalid),
        Expr(Box<Expr>),
    }
    pub struct ArrayPat {
        pub span: Span,
        pub elems: Vec<Option<Pat>>,
        pub optional: bool,
        pub type_ann: Option<Box<TsTypeAnn>>,
    }
    pub struct ObjectPat {
        pub span: Span,
        pub props: Vec<ObjectPatProp>,
        pub optional: bool,
        pub type_ann: Option<Box<TsTypeAnn>>,
    }
    pub struct AssignPat {
        pub span: Span,
        pub left: Box<Pat>,
        pub right: Box<Expr>,
        pub type_ann: Option<Box<TsTypeAnn>>,
    }
    pub struct RestPat {
        pub span: Span,
        pub dot3_token: Span,
        pub arg: Box<Pat>,
        pub type_ann: Option<Box<TsTypeAnn>>,
    }
    pub enum ObjectPatProp {
        KeyValue(KeyValuePatProp),
        Assign(AssignPatProp),
        Rest(RestPat),
    }
    pub struct KeyValuePatProp {
        #[span(lo)]
        pub key: PropName,
        #[span(hi)]
        pub value: Box<Pat>,
    }
    pub struct AssignPatProp {
        pub span: Span,
        pub key: Ident,
        pub value: Option<Box<Expr>>,
    }
    pub enum Prop {
        Shorthand(Ident),
        KeyValue(KeyValueProp),
        Assign(AssignProp),
        Getter(GetterProp),
        Setter(SetterProp),
        Method(MethodProp),
    }
    pub struct KeyValueProp {
        #[span(lo)]
        pub key: PropName,
        #[span(hi)]
        pub value: Box<Expr>,
    }
    pub struct AssignProp {
        #[span(lo)]
        pub key: Ident,
        #[span(hi)]
        pub value: Box<Expr>,
    }
    pub struct GetterProp {
        pub span: Span,
        pub key: PropName,
        pub type_ann: Option<Box<TsTypeAnn>>,
        pub body: Option<BlockStmt>,
    }
    pub struct SetterProp {
        pub span: Span,
        pub key: PropName,
        pub param: Box<Pat>,
        pub body: Option<BlockStmt>,
    }
    pub struct MethodProp {
        #[span(lo)]
        pub key: PropName,
        #[span(hi)]
        pub function: Box<Function>,
    }
    pub enum PropName {
        Ident(Ident),
        Str(Str),
        Num(Number),
        BigInt(BigInt),
        Computed(ComputedPropName),
    }
    pub struct ComputedPropName {
        pub span: Span,
        pub expr: Box<Expr>,
    }
    pub struct BlockStmt {
        pub span: Span,
        pub stmts: Vec<Stmt>,
    }
    pub enum Stmt {
        Block(BlockStmt),
        Empty(EmptyStmt),
        Debugger(DebuggerStmt),
        With(WithStmt),
        Return(ReturnStmt),
        Labeled(LabeledStmt),
        Break(BreakStmt),
        Continue(ContinueStmt),
        If(IfStmt),
        Switch(SwitchStmt),
        Throw(ThrowStmt),
        Try(Box<TryStmt>),
        While(WhileStmt),
        DoWhile(DoWhileStmt),
        For(ForStmt),
        ForIn(ForInStmt),
        ForOf(ForOfStmt),
        Decl(Decl),
        Expr(ExprStmt),
    }
    pub struct ExprStmt {
        pub span: Span,
        pub expr: Box<Expr>,
    }
    #[skip_node_id]
    pub struct EmptyStmt {
        pub span: Span,
    }
    pub struct DebuggerStmt {
        pub span: Span,
    }
    pub struct WithStmt {
        pub span: Span,
        pub obj: Box<Expr>,
        pub body: Box<Stmt>,
    }
    pub struct ReturnStmt {
        pub span: Span,
        pub arg: Option<Box<Expr>>,
    }
    pub struct LabeledStmt {
        pub span: Span,
        pub label: Ident,
        pub body: Box<Stmt>,
    }
    pub struct BreakStmt {
        pub span: Span,
        pub label: Option<Ident>,
    }
    pub struct ContinueStmt {
        pub span: Span,
        pub label: Option<Ident>,
    }
    pub struct IfStmt {
        pub span: Span,
        pub test: Box<Expr>,
        pub cons: Box<Stmt>,
        pub alt: Option<Box<Stmt>>,
    }
    pub struct SwitchStmt {
        pub span: Span,
        pub discriminant: Box<Expr>,
        pub cases: Vec<SwitchCase>,
    }
    pub struct ThrowStmt {
        pub span: Span,
        pub arg: Box<Expr>,
    }
    pub struct TryStmt {
        pub span: Span,
        pub block: BlockStmt,
        pub handler: Option<CatchClause>,
        pub finalizer: Option<BlockStmt>,
    }
    pub struct WhileStmt {
        pub span: Span,
        pub test: Box<Expr>,
        pub body: Box<Stmt>,
    }
    pub struct DoWhileStmt {
        pub span: Span,
        pub test: Box<Expr>,
        pub body: Box<Stmt>,
    }
    pub struct ForStmt {
        pub span: Span,
        pub init: Option<VarDeclOrExpr>,
        pub test: Option<Box<Expr>>,
        pub update: Option<Box<Expr>>,
        pub body: Box<Stmt>,
    }
    pub struct ForInStmt {
        pub span: Span,
        pub left: VarDeclOrPat,
        pub right: Box<Expr>,
        pub body: Box<Stmt>,
    }
    pub struct ForOfStmt {
        pub span: Span,
        pub is_await: bool,
        pub left: VarDeclOrPat,
        pub right: Box<Expr>,
        pub body: Box<Stmt>,
    }
    pub struct SwitchCase {
        pub span: Span,
        pub test: Option<Box<Expr>>,
        pub cons: Vec<Stmt>,
    }
    pub struct CatchClause {
        pub span: Span,
        pub param: Option<Pat>,
        pub body: BlockStmt,
    }
    pub enum VarDeclOrPat {
        VarDecl(Box<VarDecl>),
        Pat(Box<Pat>),
    }
    pub enum VarDeclOrExpr {
        VarDecl(Box<VarDecl>),
        Expr(Box<Expr>),
    }
    pub struct TsTypeAnn {
        pub span: Span,
        pub type_ann: Box<TsType>,
    }
    pub struct TsTypeParamDecl {
        pub span: Span,
        pub params: Vec<TsTypeParam>,
    }
    pub struct TsTypeParam {
        pub span: Span,
        pub name: Ident,
        pub constraint: Option<Box<TsType>>,
        pub default: Option<Box<TsType>>,
        pub is_in: bool,
        pub is_out: bool,
        pub is_const: bool,
    }
    pub struct TsTypeParamInstantiation {
        pub span: Span,
        pub params: Vec<Box<TsType>>,
    }

    pub struct TsParamProp {
        pub span: Span,
        pub decorators: Vec<Decorator>,
        #[not_spanned]
        pub accessibility: Option<Accessibility>,
        pub readonly: bool,
        pub param: TsParamPropParam,
        pub is_override: bool,
    }

    pub enum TsParamPropParam {
        Ident(BindingIdent),
        Assign(AssignPat),
    }
    pub struct TsQualifiedName {
        #[span(lo)]
        pub left: TsEntityName,
        #[span(hi)]
        pub right: Ident,
    }
    pub enum TsEntityName {
        TsQualifiedName(Box<TsQualifiedName>),
        Ident(Ident),
    }

    pub enum TsTypeElement {
        TsCallSignatureDecl(TsCallSignatureDecl),
        TsConstructSignatureDecl(TsConstructSignatureDecl),
        TsPropertySignature(TsPropertySignature),
        TsGetterSignature(TsGetterSignature),
        TsSetterSignature(TsSetterSignature),
        TsMethodSignature(TsMethodSignature),
        TsIndexSignature(TsIndexSignature),
    }
    pub struct TsCallSignatureDecl {
        pub span: Span,
        pub params: Vec<TsFnParam>,
        pub type_ann: Option<Box<TsTypeAnn>>,
        pub type_params: Option<Box<TsTypeParamDecl>>,
    }
    pub struct TsConstructSignatureDecl {
        pub span: Span,
        pub params: Vec<TsFnParam>,
        pub type_ann: Option<Box<TsTypeAnn>>,
        pub type_params: Option<Box<TsTypeParamDecl>>,
    }
    pub struct TsPropertySignature {
        pub span: Span,
        pub readonly: bool,
        pub key: Box<Expr>,
        pub computed: bool,
        pub optional: bool,
        pub init: Option<Box<Expr>>,
        pub params: Vec<TsFnParam>,
        pub type_ann: Option<Box<TsTypeAnn>>,
        pub type_params: Option<Box<TsTypeParamDecl>>,
    }

    pub struct TsGetterSignature {
        pub span: Span,
        pub readonly: bool,
        pub key: Box<Expr>,
        pub computed: bool,
        pub optional: bool,
        pub type_ann: Option<Box<TsTypeAnn>>,
    }

    pub struct TsSetterSignature {
        pub span: Span,
        pub readonly: bool,
        pub key: Box<Expr>,
        pub computed: bool,
        pub optional: bool,
        pub param: TsFnParam,
    }

    pub struct TsMethodSignature {
        pub span: Span,
        pub readonly: bool,
        pub key: Box<Expr>,
        pub computed: bool,
        pub optional: bool,
        pub params: Vec<TsFnParam>,
        pub type_ann: Option<Box<TsTypeAnn>>,
        pub type_params: Option<Box<TsTypeParamDecl>>,
    }
    pub struct TsIndexSignature {
        pub params: Vec<TsFnParam>,
        pub type_ann: Option<Box<TsTypeAnn>>,
        pub readonly: bool,
        pub span: Span,
        pub is_static: bool,
    }
    pub enum TsType {
        TsKeywordType(TsKeywordType),
        TsThisType(TsThisType),
        TsFnOrConstructorType(TsFnOrConstructorType),
        TsTypeRef(TsTypeRef),
        TsTypeQuery(TsTypeQuery),
        TsTypeLit(TsTypeLit),
        TsArrayType(TsArrayType),
        TsTupleType(TsTupleType),
        TsOptionalType(TsOptionalType),
        TsRestType(TsRestType),
        TsUnionOrIntersectionType(TsUnionOrIntersectionType),
        TsConditionalType(TsConditionalType),
        TsInferType(TsInferType),
        TsParenthesizedType(TsParenthesizedType),
        TsTypeOperator(TsTypeOperator),
        TsIndexedAccessType(TsIndexedAccessType),
        TsMappedType(TsMappedType),
        TsLitType(TsLitType),
        TsTypePredicate(TsTypePredicate),
        TsImportType(TsImportType),
    }
    pub enum TsFnOrConstructorType {
        TsFnType(TsFnType),
        TsConstructorType(TsConstructorType),
    }
    #[skip_node_id]
    pub struct TsKeywordType {
        pub span: Span,
        #[not_spanned]
        pub kind: TsKeywordTypeKind,
    }
    #[skip_node_id]
    pub struct TsThisType {
        pub span: Span,
    }
    pub enum TsFnParam {
        Ident(BindingIdent),
        Array(ArrayPat),
        Rest(RestPat),
        Object(ObjectPat),
    }
    pub struct TsFnType {
        pub span: Span,
        pub params: Vec<TsFnParam>,
        pub type_params: Option<Box<TsTypeParamDecl>>,
        pub type_ann: Box<TsTypeAnn>,
    }
    pub struct TsConstructorType {
        pub span: Span,
        pub params: Vec<TsFnParam>,
        pub type_params: Option<Box<TsTypeParamDecl>>,
        pub type_ann: Box<TsTypeAnn>,
        pub is_abstract: bool,
    }
    pub struct TsTypeRef {
        pub span: Span,
        pub type_name: TsEntityName,
        pub type_params: Option<Box<TsTypeParamInstantiation>>,
    }
    pub struct TsTypePredicate {
        pub span: Span,
        pub asserts: bool,
        pub param_name: TsThisTypeOrIdent,
        pub type_ann: Option<Box<TsTypeAnn>>,
    }
    pub enum TsThisTypeOrIdent {
        TsThisType(TsThisType),
        Ident(Ident),
    }
    pub struct TsTypeQuery {
        pub span: Span,
        pub expr_name: TsTypeQueryExpr,
        pub type_args: Option<Box<TsTypeParamInstantiation>>,
    }
    pub enum TsTypeQueryExpr {
        TsEntityName(TsEntityName),
        Import(TsImportType),
    }
    pub struct TsImportType {
        pub span: Span,
        pub arg: Str,
        pub qualifier: Option<TsEntityName>,
        pub type_args: Option<Box<TsTypeParamInstantiation>>,
    }
    pub struct TsTypeLit {
        pub span: Span,
        pub members: Vec<TsTypeElement>,
    }
    pub struct TsArrayType {
        pub span: Span,
        pub elem_type: Box<TsType>,
    }

    pub struct TsTupleType {
        pub span: Span,
        pub elem_types: Vec<TsTupleElement>,
    }

    pub struct TsTupleElement {
        pub span: Span,
        pub label: Option<Pat>,
        pub ty: Box<TsType>,
    }

    pub struct TsOptionalType {
        pub span: Span,
        pub type_ann: Box<TsType>,
    }
    pub struct TsRestType {
        pub span: Span,
        pub type_ann: Box<TsType>,
    }
    pub enum TsUnionOrIntersectionType {
        TsUnionType(TsUnionType),
        TsIntersectionType(TsIntersectionType),
    }
    pub struct TsUnionType {
        pub span: Span,
        pub types: Vec<Box<TsType>>,
    }
    pub struct TsIntersectionType {
        pub span: Span,
        pub types: Vec<Box<TsType>>,
    }
    pub struct TsConditionalType {
        pub span: Span,
        pub check_type: Box<TsType>,
        pub extends_type: Box<TsType>,
        pub true_type: Box<TsType>,
        pub false_type: Box<TsType>,
    }
    pub struct TsInferType {
        pub span: Span,
        pub type_param: TsTypeParam,
    }
    pub struct TsParenthesizedType {
        pub span: Span,
        pub type_ann: Box<TsType>,
    }
    pub struct TsTypeOperator {
        pub span: Span,
        #[not_spanned]
        pub op: TsTypeOperatorOp,
        pub type_ann: Box<TsType>,
    }

    pub struct TsIndexedAccessType {
        pub span: Span,
        pub readonly: bool,
        pub obj_type: Box<TsType>,
        pub index_type: Box<TsType>,
    }

    pub struct TsMappedType {
        pub span: Span,
        #[not_spanned]
        pub readonly: Option<TruePlusMinus>,
        pub type_param: TsTypeParam,
        pub name_type: Option<Box<TsType>>,
        #[not_spanned]
        pub optional: Option<TruePlusMinus>,
        pub type_ann: Option<Box<TsType>>,
    }
    pub struct TsLitType {
        pub span: Span,
        pub lit: TsLit,
    }
    pub enum TsLit {
        BigInt(BigInt),
        Number(Number),
        Str(Str),
        Bool(Bool),
        Tpl(TsTplLitType),
    }
    pub struct TsTplLitType {
        pub span: Span,
        pub types: Vec<Box<TsType>>,
        pub quasis: Vec<TplElement>,
    }
    pub struct TsInterfaceDecl {
        pub span: Span,
        pub id: Ident,
        pub declare: bool,
        pub type_params: Option<Box<TsTypeParamDecl>>,
        pub extends: Vec<TsExprWithTypeArgs>,
        pub body: TsInterfaceBody,
    }
    pub struct TsInterfaceBody {
        pub span: Span,
        pub body: Vec<TsTypeElement>,
    }
    pub struct TsExprWithTypeArgs {
        pub span: Span,
        pub expr: Box<Expr>,
        pub type_args: Option<Box<TsTypeParamInstantiation>>,
    }
    pub struct TsTypeAliasDecl {
        pub span: Span,
        pub declare: bool,
        pub id: Ident,
        pub type_params: Option<Box<TsTypeParamDecl>>,
        pub type_ann: Box<TsType>,
    }
    pub struct TsEnumDecl {
        pub span: Span,
        pub declare: bool,
        pub is_const: bool,
        pub id: Ident,
        pub members: Vec<TsEnumMember>,
    }
    pub struct TsEnumMember {
        pub span: Span,
        pub id: TsEnumMemberId,
        pub init: Option<Box<Expr>>,
    }
    pub enum TsEnumMemberId {
        Ident(Ident),
        Str(Str),
    }
    pub struct TsModuleDecl {
        pub span: Span,
        pub declare: bool,
        pub global: bool,
        pub id: TsModuleName,
        pub body: Option<TsNamespaceBody>,
    }
    pub enum TsNamespaceBody {
        TsModuleBlock(TsModuleBlock),
        TsNamespaceDecl(TsNamespaceDecl),
    }
    pub struct TsModuleBlock {
        pub span: Span,
        pub body: Vec<ModuleItem>,
    }
    pub struct TsNamespaceDecl {
        pub span: Span,
        pub declare: bool,
        pub global: bool,
        pub id: Ident,
        pub body: Box<TsNamespaceBody>,
    }
    pub enum TsModuleName {
        Ident(Ident),
        Str(Str),
    }
    pub struct TsImportEqualsDecl {
        pub span: Span,
        pub declare: bool,
        pub is_export: bool,
        pub id: Ident,
        pub module_ref: TsModuleRef,
        pub is_type_only: bool,
    }
    pub enum TsModuleRef {
        TsEntityName(TsEntityName),
        TsExternalModuleRef(TsExternalModuleRef),
    }
    pub struct TsExternalModuleRef {
        pub span: Span,
        pub expr: Str,
    }
    pub struct TsExportAssignment {
        pub span: Span,
        pub expr: Box<Expr>,
    }
    pub struct TsNamespaceExportDecl {
        pub span: Span,
        pub id: Ident,
    }
    pub struct TsAsExpr {
        pub span: Span,
        pub expr: Box<Expr>,
        pub type_ann: Box<TsType>,
    }
    pub struct TsTypeAssertion {
        pub span: Span,
        pub expr: Box<Expr>,
        pub type_ann: Box<TsType>,
    }
    pub struct TsNonNullExpr {
        pub span: Span,
        pub expr: Box<Expr>,
    }

    pub struct TsConstAssertion {
        pub span: Span,
        pub expr: Box<Expr>,
    }
    pub struct StaticBlock {
        pub span: Span,
        pub body: BlockStmt,
    }

    pub struct TsInstantiation {
        pub span: Span,
        pub expr: Box<Expr>,
        pub type_args: Box<TsTypeParamInstantiation>,
    }

    pub struct TsSatisfiesExpr {
        pub span: Span,
        pub expr: Box<Expr>,
        pub type_ann: Box<TsType>,
    }

    pub struct SuperPropExpr {
        pub span: Span,
        pub obj: Super,
        pub prop: SuperProp,
    }

    pub enum SuperProp {
        Ident(Ident),
        Computed(ComputedPropName),
    }

    pub enum Key {
        Private(PrivateName),
        Public(PropName),
    }

    pub struct AutoAccessor {
        pub span: Span,
        pub key: Key,
        pub value: Option<Box<Expr>>,
        pub type_ann: Option<Box<TsTypeAnn>>,
        pub is_static: bool,
        pub decorators: Vec<Decorator>,
        pub accessibility: Option<Accessibility>,
    }
});
