use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_ast_rnode::RTsLitType;
use stc_ts_ast_rnode::RTsNamespaceDecl;
use stc_ts_ast_rnode::RTsThisType;
use stc_ts_types::*;
use std::mem::size_of;

#[test]
fn size_of_type() {
    assert_eq!(size_of::<Type>(), 128);
}

#[test]
fn size_of_module() {
    assert_eq!(size_of::<Module>(), 96);
}

#[test]
fn size_of_class() {
    assert_eq!(size_of::<Class>(), 72);
}

#[test]
fn size_of_type_lit() {
    assert_eq!(size_of::<TypeLit>(), 40);
}

#[test]
fn size_of_interface() {
    assert_eq!(size_of::<Interface>(), 32);
}

#[test]
fn size_of_enum() {
    assert_eq!(size_of::<Enum>(), 32);
}

#[test]
fn size_of_enum_variant() {
    assert_eq!(size_of::<EnumVariant>(), 32);
}

#[test]
fn size_of_function() {
    assert_eq!(size_of::<Function>(), 32);
}

#[test]
fn size_of_constructor() {
    assert_eq!(size_of::<Constructor>(), 32);
}

#[test]
fn size_of_intersection() {
    assert_eq!(size_of::<Intersection>(), 40);
}

#[test]
fn size_of_union() {
    assert_eq!(size_of::<Union>(), 40);
}

#[test]
fn size_of_mapped() {
    assert_eq!(size_of::<Mapped>(), 80);
}

#[test]
fn size_of_optional() {
    assert_eq!(size_of::<OptionalType>(), 24);
}

#[test]
fn size_of_rest() {
    assert_eq!(size_of::<RestType>(), 24);
}

#[test]
fn size_of_symbol() {
    assert_eq!(size_of::<Symbol>(), 24);
}

#[test]
fn size_of_freezed() {
    assert_eq!(size_of::<Freezed>(), 32);
}

#[test]
fn size_of_class_instance() {
    assert_eq!(size_of::<ClassInstance>(), 32);
}

#[test]
fn size_of_alias() {
    assert_eq!(size_of::<Alias>(), 32);
}

#[test]
fn size_of_namespace() {
    assert_eq!(size_of::<RTsNamespaceDecl>(), 32);
}

#[test]
fn size_of_operator() {
    assert_eq!(size_of::<Operator>(), 32);
}

#[test]
fn size_of_array() {
    assert_eq!(size_of::<Array>(), 32);
}

#[test]
fn size_of_tuple() {
    assert_eq!(size_of::<Tuple>(), 40);
}

#[test]
fn size_of_conditional() {
    assert_eq!(size_of::<Tuple>(), 32);
}

#[test]
fn size_of_keyword() {
    assert_eq!(size_of::<RTsKeywordType>(), 32);
}

#[test]
fn size_of_lit() {
    assert_eq!(size_of::<RTsLitType>(), 32);
}

#[test]
fn size_of_ref() {
    assert_eq!(size_of::<Ref>(), 32);
}

#[test]
fn size_of_indexed_access_type() {
    assert_eq!(size_of::<IndexedAccessType>(), 32);
}

#[test]
fn size_of_predicate() {
    assert_eq!(size_of::<Predicate>(), 32);
}

#[test]
fn size_of_import() {
    assert_eq!(size_of::<Predicate>(), 32);
}

#[test]
fn size_of_infer() {
    assert_eq!(size_of::<InferType>(), 32);
}

#[test]
fn size_of_query() {
    assert_eq!(size_of::<QueryType>(), 32);
}

#[test]
fn size_of_this() {
    assert_eq!(size_of::<RTsThisType>(), 12);
}

#[test]
fn size_of_static_this() {
    assert_eq!(size_of::<StaticThis>(), 12);
}
