//! Type forms for generic inference.
#![deny(missing_debug_implementations)]
#![deny(unreachable_code)]
#![feature(box_syntax)]

use stc_ts_types::{name::Name, Type};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Var {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeForm {
    // Type parameters, including infer type.
    Wildcard(Var),
    /// [Type::Keyword], [Type::Lit]
    Element,
    Array {
        elem: Box<TypeForm>,
    },
    Object {},
    Ref {
        type_name: Name,
        type_args: Vec<TypeForm>,
    },
    Fn {
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

            Type::Conditional(ty) => Self::Conditional {
                truthy: box Self::from(&ty.true_type),
                falsy: box Self::from(&ty.false_type),
            },

            Type::TypeLit(_) => todo!(),

            Type::IndexedAccessType(_) => todo!(),
            Type::Tuple(_) => todo!(),

            Type::Function(_) => todo!(),
            Type::Constructor(_) => todo!(),
            Type::Operator(_) => todo!(),
            Type::EnumVariant(_) => todo!(),
            Type::Interface(_) => todo!(),
            Type::Mapped(_) => todo!(),

            Type::Rest(_) => todo!(),
            Type::Optional(_) => todo!(),

            Type::Param(_) => todo!(),
            Type::Infer(_) => todo!(),

            Type::Union(_) | Type::Intersection(_) => {
                // Actually unreahable.
                Self::Element
            }

            _ => Self::Element,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypePath {
    ReturnType,
    FnParam,
}

/// Returns path to deepest match.
pub fn compare(a: &TypeForm, b: &TypeForm) -> Vec<TypePath> {}
