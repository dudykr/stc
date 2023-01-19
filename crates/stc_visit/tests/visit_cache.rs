use std::{cell::RefCell, rc::Rc};

use crate::{visit_cache, Fold, FoldWith, Visit, VisitWith};

#[derive(Visit)]
struct Deep {
    inner: Option<Box<Deep>>,
    vec: Vec<Deep>,
}

#[test]
fn test_fold_1() {
    let data = Deep {
        inner: Some(Box::new(Deep {
            inner: None,
            vec: vec![Deep {
                inner: None,
                vec: vec![Deep { inner: None, vec: vec![] }],
            }],
        })),
        vec: vec![Deep {
            inner: None,
            vec: vec![Deep { inner: None, vec: vec![] }],
        }],
    };

    let mut folder = Folder {
        count: Rc::new(RefCell::new(0)),
    };

    let result = data.fold_with(&mut folder);
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
        assert!(FOUND.is_set());
        value
    }
}

impl Visit<String> for Visitor {
    fn visit(&mut self, v: &String) {
        assert!(FOUND.is_set());

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
