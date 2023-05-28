#![allow(incomplete_features)]
#![feature(specialization)]

use std::{cell::RefCell, rc::Rc};

use stc_visit::{visit_cache, Fold, FoldWith, Visit, VisitWith};

#[derive(Debug, Visit)]
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
                inner: Some(Box::new(Deep {
                    inner: None,
                    vec: vec![Deep {
                        inner: Some(Box::new(Deep {
                            inner: None,
                            vec: vec![Deep {
                                inner: None,
                                vec: vec![Deep { inner: None, vec: vec![] }],
                            }],
                        })),
                        vec: vec![Deep { inner: None, vec: vec![] }],
                    }],
                })),
                vec: vec![Deep { inner: None, vec: vec![] }],
            }],
        })),
        vec: vec![Deep {
            inner: None,
            vec: vec![Deep {
                inner: None,
                vec: vec![
                    Deep {
                        inner: None,
                        vec: vec![Deep {
                            inner: Some(Box::new(Deep {
                                inner: None,
                                vec: vec![Deep {
                                    inner: Some(Box::new(Deep {
                                        inner: None,
                                        vec: vec![Deep {
                                            inner: None,
                                            vec: vec![Deep { inner: None, vec: vec![] }],
                                        }],
                                    })),
                                    vec: vec![Deep { inner: None, vec: vec![] }],
                                }],
                            })),
                            vec: vec![Deep { inner: None, vec: vec![] }],
                        }],
                    },
                    Deep {
                        inner: None,
                        vec: vec![Deep {
                            inner: Some(Box::new(Deep {
                                inner: None,
                                vec: vec![Deep {
                                    inner: Some(Box::new(Deep {
                                        inner: None,
                                        vec: vec![Deep {
                                            inner: None,
                                            vec: vec![Deep { inner: None, vec: vec![] }],
                                        }],
                                    })),
                                    vec: vec![Deep { inner: None, vec: vec![] }],
                                }],
                            })),
                            vec: vec![Deep { inner: None, vec: vec![] }],
                        }],
                    },
                ],
            }],
        }],
    };

    let mut folder = Folder {
        count: Rc::new(RefCell::new(0)),
    };

    let result = data.fold_with(&mut folder);
    dbg!(&result);

    assert_eq!(*folder.count.borrow(), 30);
}

visit_cache!(static FOUND: bool);

struct Folder {
    count: Rc<RefCell<usize>>,
}

struct Visitor {
    calc_count: Rc<RefCell<usize>>,

    found: bool,
}

impl Fold<Vec<Vec<Deep>>> for Folder {
    fn fold(&mut self, value: Vec<Vec<Deep>>) -> Vec<Vec<Deep>> {
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

impl Fold<Vec<Deep>> for Folder {
    fn fold(&mut self, value: Vec<Deep>) -> Vec<Deep> {
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

impl Fold<Deep> for Folder {
    fn fold(&mut self, value: Deep) -> Deep {
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

impl Visit<Deep> for Visitor {
    fn visit(&mut self, v: &Deep) {
        assert!(FOUND.is_set());

        dbg!("Visit");

        let key = v as *const Deep as *const ();

        dbg!(key);
        if FOUND.get_copied(key).is_some() {
            dbg!("cached");
            return;
        }

        *self.calc_count.borrow_mut() += 1;
        let result = v.inner.is_none() && v.vec.is_empty();

        FOUND.insert(key, result);

        v.visit_children_with(self);
    }
}
