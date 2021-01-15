use crate::analyzer::Analyzer;
use crate::analyzer::ScopeKind;
use crate::validator::ValidateWith;
use crate::ValidationResult;
use stc_ts_ast_rnode::RObjectLit;
use stc_ts_ast_rnode::RPropOrSpread;
use stc_ts_ast_rnode::RSpreadElement;
use stc_ts_file_analyzer_macros::validator;
use stc_ts_types::Type;
use stc_ts_types::TypeElement;
use stc_ts_types::TypeLit;
use stc_ts_types::Union;

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RObjectLit) -> ValidationResult {
        self.with_child(
            ScopeKind::ObjectLit,
            Default::default(),
            |a: &mut Analyzer| {
                let mut ty = box Type::TypeLit(TypeLit {
                    span: node.span,
                    members: vec![],
                });

                for prop in node.props.iter() {
                    ty = a.append_prop_or_spread_to_type(ty, prop)?;
                }

                Ok(ty)
            },
        )
    }
}

impl Analyzer<'_, '_> {
    fn append_prop_or_spread_to_type(
        &mut self,
        to: Box<Type>,
        prop: &RPropOrSpread,
    ) -> ValidationResult {
        match prop {
            RPropOrSpread::Spread(RSpreadElement { expr, .. }) => {
                let prop_ty: Box<Type> = expr.validate_with_default(self)?;
                self.append_type(to, prop_ty)
            }
            RPropOrSpread::Prop(prop) => {
                let p: TypeElement = prop.validate_with(self)?;
                self.append_type_element(to, p)
            }
        }
    }

    /// If rhs is an union type, return type will be union.
    ///
    /// `{ a: number } + ( {b: number} | { c: number } )` => `{ a: number, b:
    /// number } | { a: number, c: number }`
    fn append_type(&mut self, to: Box<Type>, rhs: Box<Type>) -> ValidationResult<Box<Type>> {
        if to.is_any() || to.is_unknown() {
            return Ok(to);
        }
        if rhs.is_any() || rhs.is_unknown() {
            return Ok(to);
        }
        let to = to.foldable();
        match to {
            Type::Union(to) => Ok(box Type::Union(Union {
                span: to.span,
                types: to
                    .types
                    .into_iter()
                    .map(|to| self.append_type(to, rhs.clone()))
                    .collect::<Result<_, _>>()?,
            })),
            _ => {
                unimplemented!("append type: {:?} <= {:?}", to, rhs)
            }
        }
    }

    fn append_type_element(
        &mut self,
        to: Box<Type>,
        rhs: TypeElement,
    ) -> ValidationResult<Box<Type>> {
        if to.is_any() || to.is_unknown() {
            return Ok(to);
        }
        let to = to.foldable();

        match to {
            Type::Union(to) => Ok(box Type::Union(Union {
                span: to.span,
                types: to
                    .types
                    .into_iter()
                    .map(|to| self.append_type_element(to, rhs.clone()))
                    .collect::<Result<_, _>>()?,
            })),
            _ => {
                unimplemented!("append type element: {:?} <= {:?}", to, rhs)
            }
        }
    }
}
