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

    use crate::{visit_cache, Fold, FoldWith, Visit, VisitWith};

    #[test]
    fn test_fold_1() {
        let strings: Vec<Vec<String>> = vec![
            vec!["aaaa".into(), "aabb".into(), "bb".into(), "bbbb".into()],
            vec!["a".into(), "b".into(), "c".into(), "d".into(), "e".into()],
            vec![],
            vec!["aa".into()],
            vec![],
            vec![],
            vec![],
            vec![],
        ];

        let mut folder = Folder {
            count: Rc::new(RefCell::new(0)),
        };

        let result = strings.fold_with(&mut folder);
        assert_eq!(*folder.count.borrow(), 4);
    }

    visit_cache!(static FOUND: bool);

    struct Folder {
        count: Rc<RefCell<usize>>,
    }

    struct Visitor {
        calc_count: Rc<RefCell<usize>>,

        found: bool,
    }

    impl Fold<Vec<Vec<String>>> for Folder {
        fn fold(&mut self, value: Vec<Vec<String>>) -> Vec<Vec<String>> {
            FOUND.configure(|| {
                let mut visitor = Visitor {
                    calc_count: self.count.clone(),
                    found: false,
                };

                value.visit_with(&mut visitor);
                if !visitor.found {
                    return value;
                }

                value.fold_children_with(self)
            })
        }
    }

    impl Fold<Vec<String>> for Folder {
        fn fold(&mut self, value: Vec<String>) -> Vec<String> {
            FOUND.configure(|| {
                let mut visitor = Visitor {
                    calc_count: self.count.clone(),
                    found: false,
                };

                value.visit_with(&mut visitor);
                if !visitor.found {
                    return value;
                }

                value.fold_children_with(self)
            })
        }
    }

    impl Fold<String> for Folder {
        fn fold(&mut self, value: String) -> String {
            FOUND.configure(|| {
                let mut visitor = Visitor {
                    calc_count: self.count.clone(),
                    found: false,
                };

                value.visit_with(&mut visitor);

                value
            })
        }
    }

    impl Visit<String> for Visitor {
        fn visit(&mut self, v: &String) {
            dbg!("Visit");

            let key = v as *const String as *const ();

            dbg!(key);
            if let Some(..) = FOUND.get_copied(key) {
                dbg!("cached");
                return;
            }

            *self.calc_count.borrow_mut() += 1;
            let result = v.contains("aa");

            FOUND.insert(key, result);
        }
    }
}
