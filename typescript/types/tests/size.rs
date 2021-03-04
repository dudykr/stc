use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_ast_rnode::RTsLitType;
use stc_ts_ast_rnode::RTsNamespaceDecl;
use stc_ts_ast_rnode::RTsThisType;
use std::mem::size_of;

#[test]
fn size_of_namespace() {
    assert_eq!(size_of::<RTsNamespaceDecl>(), 64);
}

#[test]
fn size_of_keyword() {
    assert_eq!(size_of::<RTsKeywordType>(), 16);
}

#[test]
fn size_of_lit() {
    assert_eq!(size_of::<RTsLitType>(), 104);
}

#[test]
fn size_of_this() {
    assert_eq!(size_of::<RTsThisType>(), 12);
}
