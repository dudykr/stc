//! Smarter version of `derive(Is)`
#![allow(deprecated)]

use debug_unreachable::debug_unreachable;

use crate::*;

macro_rules! impl_is {
    ($variant:ident,$type_name:ident, $is_name:ident,$as_name:ident,$as_mut_name:ident,$opt_name:ident,$expect_name:ident) => {
        impl Type {
            pub fn $is_name(&self) -> bool {
                matches!(self.normalize(), Type::$variant(_))
            }

            pub fn $as_name(&self) -> Option<&$type_name> {
                match self.normalize() {
                    Type::$variant(ty) => Some(ty),
                    _ => None,
                }
            }

            /// This method normalizes the type **only if** the underlying type is the
            /// required variant.
            pub fn $as_mut_name(&mut self) -> Option<&mut $type_name> {
                if self.$is_name() {
                    match self.normalize_mut() {
                        Type::$variant(ty) => Some(ty),
                        _ => unsafe {
                            debug_unreachable!("`$is_name` is true, so this branch is unreachable")
                        },
                    }
                } else {
                    None
                }
            }

            /// This method normalizes the type **only if** the underlying type is the
            /// required variant.
            pub fn $opt_name(mut self) -> Option<$type_name> {
                if self.$is_name() {
                    self.normalize_mut();
                    match self {
                        Type::$variant(ty) => Some(ty),
                        _ => unsafe {
                            debug_unreachable!("`$is_name` is true, so this branch is unreachable")
                        },
                    }
                } else {
                    None
                }
            }

            /// # Panics
            ///
            /// Panics if the underlying type is not the required variant.
            pub fn $expect_name(mut self) -> $type_name {
                self.$opt_name()
                    .expect(concat!("expected ", stringify!($variant)))
            }
        }
    };
}

impl_is!(
    This,
    ThisType,
    is_this,
    as_this,
    as_this_mut,
    this,
    expect_this
);
impl_is!(Lit, LitType, is_lit, as_lit, as_lit_mut, lit, expect_lit);
impl_is!(
    Query,
    QueryType,
    is_query,
    as_query,
    as_query_mut,
    query,
    expect_query
);
impl_is!(
    Infer,
    InferType,
    is_infer,
    as_infer,
    as_infer_mut,
    infer,
    expect_infer
);
impl_is!(
    Import,
    ImportType,
    is_import,
    as_import,
    as_import_mut,
    import,
    expect_import
);
impl_is!(
    Predicate,
    Predicate,
    is_predicate,
    as_predicate,
    as_predicate_mut,
    predicate,
    expect_predicate
);
impl_is!(
    IndexedAccessType,
    IndexedAccessType,
    is_indexed_access_type,
    as_indexed_access_type,
    as_indexed_access_type_mut,
    indexed_access_type,
    expect_indexed_access_type
);
impl_is!(
    Ref,
    Ref,
    is_ref_type,
    as_ref_type,
    as_ref_type_mut,
    ref_type,
    expect_ref_type
);
impl_is!(
    TypeLit,
    TypeLit,
    is_type_lit,
    as_type_lit,
    as_type_lit_mut,
    type_lit,
    expect_type_lit
);
impl_is!(
    Keyword,
    KeywordType,
    is_keyword,
    as_keyword,
    as_keyword_mut,
    keyword,
    expect_keyword
);
impl_is!(
    Conditional,
    Conditional,
    is_conditional,
    as_conditional,
    as_conditional_mut,
    conditional,
    expect_conditional
);
impl_is!(
    Tuple,
    Tuple,
    is_tuple,
    as_tuple,
    as_tuple_mut,
    tuple,
    expect_tuple
);
impl_is!(
    Array,
    Array,
    is_array,
    as_array,
    as_array_mut,
    array,
    expect_array
);
impl_is!(
    Union,
    Union,
    is_union_type,
    as_union_type,
    as_union_type_mut,
    union_type,
    expect_union_type
);
impl_is!(
    Intersection,
    Intersection,
    is_intersection,
    as_intersection,
    as_intersection_mut,
    intersection,
    expect_intersection
);
impl_is!(
    Function,
    Function,
    is_fn_type,
    as_fn_type,
    as_fn_type_mut,
    fn_type,
    expect_fn_type
);
impl_is!(
    Constructor,
    Constructor,
    is_constructor,
    as_constructor,
    as_constructor_mut,
    constructor,
    expect_constructor
);
impl_is!(
    Operator,
    Operator,
    is_operator,
    as_operator,
    as_operator_mut,
    operator,
    expect_operator
);
impl_is!(
    Param,
    TypeParam,
    is_param,
    as_param,
    as_param_mut,
    param,
    expect_param
);
impl_is!(
    EnumVariant,
    EnumVariant,
    is_enum_variant,
    as_enum_variant,
    as_enum_variant_mut,
    enum_variant,
    expect_enum_variant
);
impl_is!(
    Interface,
    Interface,
    is_interface,
    as_interface,
    as_interface_mut,
    interface,
    expect_interface
);
impl_is!(
    Enum,
    Enum,
    is_enum_type,
    as_enum_type,
    as_enum_type_mut,
    enum_type,
    expect_enum_type
);
impl_is!(
    Mapped,
    Mapped,
    is_mapped,
    as_mapped,
    as_mapped_mut,
    mapped,
    expect_mapped
);
impl_is!(
    Alias,
    Alias,
    is_alias,
    as_alias,
    as_alias_mut,
    alias,
    expect_alias
);
impl_is!(
    Namespace,
    Namespace,
    is_namespace,
    as_namespace,
    as_namespace_mut,
    namespace,
    expect_namespace
);
impl_is!(
    Module,
    Module,
    is_module,
    as_module,
    as_module_mut,
    module,
    expect_module
);
impl_is!(
    Class,
    Class,
    is_class,
    as_class,
    as_class_mut,
    class,
    expect_class
);
impl_is!(
    ClassDef,
    ClassDef,
    is_class_def,
    as_class_def,
    as_class_def_mut,
    class_def,
    expect_class_def
);
impl_is!(
    Rest,
    RestType,
    is_rest,
    as_rest,
    as_rest_mut,
    rest,
    expect_rest
);
impl_is!(
    Optional,
    OptionalType,
    is_optional,
    as_optional,
    as_optional_mut,
    optional,
    expect_optional
);
impl_is!(
    Symbol,
    Symbol,
    is_symbol,
    as_symbol,
    as_symbol_mut,
    symbol,
    expect_symbol
);
impl_is!(Tpl, TplType, is_tpl, as_tpl, as_tpl_mut, tpl, expect_tpl);
impl_is!(
    Intrinsic,
    Intrinsic,
    is_intrinsic,
    as_intrinsic,
    as_intrinsic_mut,
    intrinsic,
    expect_intrinsic
);
