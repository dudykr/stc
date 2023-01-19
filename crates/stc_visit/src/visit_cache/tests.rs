use crate::visit_cache;

#[test]
fn test_configure() {
    visit_cache!(static FOUND: bool);

    assert!(!FOUND.is_set());
    FOUND.configure(|| {
        assert!(FOUND.is_set());
    });
    assert!(!FOUND.is_set());
}
