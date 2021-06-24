//! Type forms for generic inference.
#![deny(missing_debug_implementations)]
#![deny(unreachable_code)]
#![feature(cmp_min_max_by)]
#![feature(box_syntax)]

use itertools::{EitherOrBoth, Itertools};
use stc_ts_types::{name::Name, Type};
use std::cmp::{max_by, Ordering};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Var {}

/// `Form` of the type.
///
/// For example, `T | PromiseLike<T>` has identical `form` with `void |
/// PrmomiseLike<void>`
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeForm {
    // Type parameters, including infer type.
    Wildcard(Var),
    /// [Type::Keyword], [Type::Lit]
    Element,
    Array {
        elem: Box<TypeForm>,
    },
    Tuple {
        elems: Vec<TypeForm>,
    },
    Object {
        /// TODO: `props: Vec<(Key, TypeForm)>`
        prop_count: usize,
    },
    Ref {
        type_name: Name,
        type_args: Vec<TypeForm>,
    },
    Fn {
        params: Vec<TypeForm>,
        return_type: Box<TypeForm>,
    },
    Constructor {
        params: Vec<TypeForm>,
        return_type: Box<TypeForm>,
    },
    Instance {
        of: Box<TypeForm>,
    },
    Conditional {
        truthy: Box<TypeForm>,
        falsy: Box<TypeForm>,
    },
}

impl From<&Box<Type>> for TypeForm {
    #[inline]
    fn from(ty: &Box<Type>) -> Self {
        (&**ty).into()
    }
}

impl From<&Type> for TypeForm {
    fn from(ty: &Type) -> Self {
        match ty {
            Type::Instance(ty) => Self::Instance {
                of: box Self::from(&ty.ty),
            },

            Type::Ref(ty) => Self::Ref {
                type_name: ty.type_name.clone().into(),
                type_args: ty
                    .type_args
                    .as_ref()
                    .map(|v| &*v.params)
                    .into_iter()
                    .flatten()
                    .map(Self::from)
                    .collect(),
            },

            Type::Array(ty) => Self::Array {
                elem: box Self::from(&ty.elem_type),
            },

            Type::Tuple(ty) => Self::Tuple {
                elems: ty.elems.iter().map(|el| &el.ty).map(Self::from).collect(),
            },

            Type::Conditional(ty) => Self::Conditional {
                truthy: box Self::from(&ty.true_type),
                falsy: box Self::from(&ty.false_type),
            },

            Type::TypeLit(ty) => Self::Object {
                prop_count: ty.members.len(),
            },

            Type::Function(ty) => Self::Fn {
                params: ty.params.iter().map(|p| &p.ty).map(Self::from).collect(),
                return_type: box Self::from(&ty.ret_ty),
            },

            Type::Constructor(ty) => Self::Constructor {
                params: ty.params.iter().map(|p| &p.ty).map(Self::from).collect(),
                return_type: box Self::from(&ty.type_ann),
            },

            Type::Param(_) => TypeForm::Wildcard(Var {}),
            Type::Infer(_) => TypeForm::Wildcard(Var {}),

            // TODO
            Type::Optional(ty) => TypeForm::from(&ty.ty),

            // TODO
            Type::Operator(_) | Type::Rest(_) => TypeForm::Element,

            Type::Union(_) | Type::Intersection(_) => {
                // Actually unreahable.
                Self::Element
            }

            _ => Self::Element,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypePath {
    TypeName,
    Cond,
    ArrayElem,
    ReturnType,
    FnParam,
    /// Element of a conditional type.
    CondType,
    Instance,
    Wildcard,
    ObjectPropCountMatch,
}

/// Returns path to deepest match.
pub fn compare(a: &TypeForm, b: &TypeForm) -> Vec<TypePath> {
    let mut ctx = Ctx::default();
    ctx.compare(a, b)
}

#[derive(Debug, Default)]
struct Ctx {
    path: Vec<TypePath>,
}

impl Ctx {
    fn compare_with_path(&mut self, path: TypePath, a: &TypeForm, b: &TypeForm) -> Vec<TypePath> {
        self.path.push(path);
        let res = self.compare(a, b);
        let popped = self.path.pop();
        debug_assert_eq!(popped, Some(path));
        res
    }

    fn compare(&mut self, a: &TypeForm, b: &TypeForm) -> Vec<TypePath> {
        match (a, b) {
            (TypeForm::Wildcard(_), _) | (_, TypeForm::Wildcard(_)) => {
                let mut path = self.path.clone();
                path.push(TypePath::Wildcard);
                return path;
            }
            (TypeForm::Element, TypeForm::Element) => {
                return self.path.clone();
            }

            (TypeForm::Instance { of: a }, TypeForm::Instance { of: b }) => {
                return self.compare_with_path(TypePath::Instance, &a, &b)
            }

            (TypeForm::Array { elem: a }, TypeForm::Array { elem: b }) => {
                return self.compare_with_path(TypePath::ArrayElem, &a, &b);
            }

            (TypeForm::Tuple { elems: a }, TypeForm::Tuple { elems: b }) => {
                if a.len() != b.len() {
                    return vec![];
                }

                let result = a
                    .iter()
                    .zip(b.iter())
                    .map(|(a, b)| self.compare_with_path(TypePath::ArrayElem, a, b))
                    .max_by_key(|v| v.len());

                result.unwrap_or_default()
            }

            (TypeForm::Object { prop_count: a }, TypeForm::Object { prop_count: b }) => {
                if a != b {
                    return Default::default();
                }
                let mut path = self.path.clone();
                path.push(TypePath::ObjectPropCountMatch);
                return path;
            }

            (
                TypeForm::Ref {
                    type_name: a_name,
                    type_args: a_args,
                },
                TypeForm::Ref {
                    type_name: b_name,
                    type_args: b_args,
                },
            ) => {
                if a_name != b_name {
                    return Default::default();
                }

                a_args
                    .iter()
                    .zip_longest(b_args.iter())
                    .filter_map(|pair| match pair {
                        EitherOrBoth::Both(a, b) => Some(self.compare_with_path(TypePath::TypeName, a, b)),
                        _ => None,
                    })
                    .max_by_key(|v| v.len())
                    .unwrap_or_default()
            }

            (
                TypeForm::Fn {
                    params: a_parmas,
                    return_type: a_return_yype,
                },
                TypeForm::Fn {
                    params: b_parmas,
                    return_type: b_return_yype,
                },
            )
            | (
                TypeForm::Constructor {
                    params: a_parmas,
                    return_type: a_return_yype,
                },
                TypeForm::Constructor {
                    params: b_parmas,
                    return_type: b_return_yype,
                },
            ) => {
                let ret_path = self.compare_with_path(TypePath::ReturnType, &a_return_yype, &b_return_yype);

                let params_path = a_parmas
                    .iter()
                    .zip_longest(b_parmas.iter())
                    .filter_map(|pair| match pair {
                        EitherOrBoth::Both(a, b) => Some(self.compare_with_path(TypePath::TypeName, a, b)),
                        _ => None,
                    })
                    .max_by(max_path)
                    .unwrap_or_default();

                max_by(ret_path, params_path, max_path)
            }

            (
                TypeForm::Conditional {
                    truthy: a_truthy,
                    falsy: a_falsy,
                },
                TypeForm::Conditional {
                    truthy: b_truthy,
                    falsy: b_falsy,
                },
            ) => {
                let mut max = vec![];
                for a in &[a_truthy, a_falsy] {
                    for b in &[b_truthy, b_falsy] {
                        max = max_by(max, self.compare_with_path(TypePath::Cond, &a, &b), max_path);
                    }
                }

                max
            }

            (
                TypeForm::Conditional {
                    truthy: a_truthy,
                    falsy: a_falsy,
                },
                b,
            ) => {
                let mut max = vec![];
                for a in &[a_truthy, a_falsy] {
                    max = max_by(max, self.compare_with_path(TypePath::Cond, &a, &b), max_path);
                }

                max
            }
            (
                a,
                TypeForm::Conditional {
                    truthy: b_truthy,
                    falsy: b_falsy,
                },
            ) => {
                let mut max = vec![];
                for b in &[b_truthy, b_falsy] {
                    max = max_by(max, self.compare_with_path(TypePath::Cond, &a, &b), max_path);
                }

                max
            }

            (
                TypeForm::Array { .. }
                | TypeForm::Tuple { .. }
                | TypeForm::Object { .. }
                | TypeForm::Ref { .. }
                | TypeForm::Fn { .. }
                | TypeForm::Constructor { .. }
                | TypeForm::Instance { .. },
                _,
            )
            | (
                _,
                TypeForm::Array { .. }
                | TypeForm::Tuple { .. }
                | TypeForm::Object { .. }
                | TypeForm::Ref { .. }
                | TypeForm::Fn { .. }
                | TypeForm::Constructor { .. }
                | TypeForm::Instance { .. },
            ) => return vec![],
        }
    }
}

fn max_path(a: &Vec<TypePath>, b: &Vec<TypePath>) -> Ordering {
    if a.len() > b.len() {
        return Ordering::Greater;
    }
    if b.len() > a.len() {
        return Ordering::Less;
    }

    a.cmp(b)
}

pub fn max_index(v: &[Vec<TypePath>]) -> Option<usize> {
    let mut iter = v.iter().enumerate();
    let init = iter.next()?;
    iter.try_fold(init, |acc, x| {
        // return None if x is NaN
        let cmp = max_path(&x.1, acc.1);
        // if x is greater the acc
        let max = if let std::cmp::Ordering::Greater = cmp { x } else { acc };
        Some(max)
    })
    .map(|v| v.0)
}
