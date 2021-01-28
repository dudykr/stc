use super::TypeOfMode;
use crate::analyzer::Analyzer;
use crate::util::type_ext::TypeVecExt;
use crate::validator;
use crate::validator::ValidateWith;
use crate::ValidationResult;
use stc_ts_ast_rnode::RArrayLit;
use stc_ts_ast_rnode::RExprOrSpread;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_types::Array;
use stc_ts_types::Tuple;
use stc_ts_types::TupleElement;
use stc_ts_types::Type;
use stc_ts_types::TypeParamInstantiation;
use swc_common::Spanned;
use swc_ecma_ast::TsKeywordTypeKind;

#[validator]
impl Analyzer<'_, '_> {
    fn validate(
        &mut self,
        arr: &RArrayLit,
        mode: TypeOfMode,
        type_args: Option<&TypeParamInstantiation>,
        type_ann: Option<&Type>,
    ) -> ValidationResult {
        let span = arr.span;
        let elems = &arr.elems;

        let prefer_tuple = self.prefer_tuple(type_ann);
        let mut can_be_tuple = true;
        let mut elements = Vec::with_capacity(elems.len());

        for elem in elems.iter() {
            let span = elem.span();
            let ty = match elem {
                Some(RExprOrSpread { spread: None, ref expr }) => {
                    let ty = expr.validate_with_default(self)?;
                    match &*ty {
                        Type::TypeLit(..) => {
                            can_be_tuple = false;
                        }
                        _ => {}
                    }
                    ty
                }
                Some(RExprOrSpread { spread: Some(..), expr }) => {
                    let element_type = expr.validate_with_default(self)?;
                    let element_type = box element_type.foldable();

                    match *element_type {
                        Type::Array(array) => {
                            can_be_tuple = false;
                            elements.push(TupleElement {
                                span,
                                label: None,
                                ty: array.elem_type,
                            });
                        }
                        Type::Tuple(tuple) => {
                            if !prefer_tuple {
                                can_be_tuple = false;
                            }
                            elements.extend(tuple.elems);
                        }
                        Type::Keyword(RTsKeywordType {
                            kind: TsKeywordTypeKind::TsAnyKeyword,
                            ..
                        }) => {
                            can_be_tuple = false;
                            elements.push(TupleElement {
                                span,
                                label: None,
                                ty: element_type.clone(),
                            });
                        }
                        _ => {
                            unimplemented!("type of array spread: {:?}", element_type)
                        }
                    }
                    continue;
                }
                None => {
                    let ty = Type::undefined(span);
                    ty
                }
            };
            elements.push(TupleElement { span, label: None, ty });
        }

        if self.ctx.in_export_default_expr && elements.is_empty() {
            return Ok(box Type::Array(Array {
                span,
                elem_type: Type::any(span),
            }));
        }

        if !can_be_tuple {
            let mut types: Vec<_> = elements.into_iter().map(|element| element.ty).collect();
            types.dedup_type();

            let mut ty = box Type::Array(Array {
                span,
                elem_type: Type::union(types),
            });
            self.normalize_union_of_objects(&mut ty);

            return Ok(ty);
        }

        return Ok(box Type::Tuple(Tuple { span, elems: elements }));
    }
}

impl Analyzer<'_, '_> {}
