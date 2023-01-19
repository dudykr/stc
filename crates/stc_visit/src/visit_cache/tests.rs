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

mod fold_1 {
    use std::{cell::RefCell, rc::Rc};

    use crate::{visit_cache, Fold};

    #[test]
    fn test_fold_1() {}

    visit_cache!(static FOUND: bool);

    struct Folder {}

    struct Visitor {
        count: Rc<RefCell<usize>>,
    }

    impl Fold<Vec<String>> for Folder {}
}
