use crate::analyzer::Analyzer;
use crate::ValidationResult;
use stc_ts_errors::debug::dump_type_as_string;
use stc_ts_errors::DebugExt;
use stc_ts_types::Key;
use stc_ts_types::Mapped;
use stc_ts_types::Operator;
use stc_ts_types::PropertySignature;
use stc_ts_types::Type;
use stc_ts_types::TypeElement;
use stc_ts_types::TypeLit;
use std::borrow::Cow;
use swc_common::Span;
use swc_common::Spanned;
use swc_common::TypeEq;
use swc_ecma_ast::TruePlusMinus;
use swc_ecma_ast::TsTypeOperatorOp;

impl Analyzer<'_, '_> {
    /// Required because mapped type can specified by user, like

    ///
    /// ```ts
    /// declare const a: Partial<Foo>;
    /// ```
    pub(crate) fn expand_mapped(&mut self, span: Span, m: &Mapped) -> ValidationResult {
        match m.type_param.constraint.as_deref().map(|v| v.normalize()) {
            Some(Type::Operator(Operator {
                op: TsTypeOperatorOp::KeyOf,
                ty,
                ..
            })) => {
                if let Some(mapped_ty) = m.ty.as_deref().map(Type::normalize) {
                    // Special case, but many usages can be handled with this check.
                    if ty.normalize().type_eq(mapped_ty) {
                        let mut new_type = self
                            .type_to_type_lit(span, &ty)
                            .context("tried to convert a type to type literal to expand mapped type")?
                            .map(Cow::into_owned);

                        if let Some(mut new) = new_type {
                            for member in &mut new.members {
                                self.apply_mapped_flags(member, m.optional, m.readonly);
                            }

                            return Ok(Type::TypeLit(new));
                        }
                    }
                }

                // TODO: Verify that m.ty does not contain key type.
                let keys = self.extract_keys_as_keys(span, ty)?;
                let members = keys
                    .into_iter()
                    .map(|key| PropertySignature {
                        span: key.span(),
                        key,
                        type_ann: m.ty.clone(),
                        readonly: false,
                        optional: false,
                        params: Default::default(),
                        type_params: Default::default(),
                    })
                    .map(TypeElement::Property)
                    .map(|mut el| {
                        self.apply_mapped_flags(&mut el, m.optional, m.readonly);
                        el
                    })
                    .collect();

                return Ok(Type::TypeLit(TypeLit {
                    span: m.span,
                    members,
                    metadata: Default::default(),
                }));
            }
            _ => {}
        }

        unimplemented!(
            "expand_mapped: {}",
            dump_type_as_string(&self.cm, &Type::Mapped(m.clone()))
        )
    }

    fn extract_keys_as_keys(&mut self, span: Span, ty: &Type) -> ValidationResult<Vec<Key>> {
        let ty = ty.normalize();

        match ty {
            Type::Ref(..) => {
                let ty = self.expand_top_ref(span, Cow::Borrowed(ty))?;
                return self.extract_keys_as_keys(span, &ty);
            }
            Type::TypeLit(ty) => {
                let mut keys = vec![];
                for m in &ty.members {
                    match m {
                        TypeElement::Call(_) => {}
                        TypeElement::Constructor(_) => {}
                        TypeElement::Property(p) => {
                            keys.push(p.key.clone());
                        }
                        TypeElement::Method(m) => {
                            keys.push(m.key.clone());
                        }
                        TypeElement::Index(_) => {}
                    }
                }

                return Ok(keys);
            }
            Type::Interface(ty) => {
                let mut keys = vec![];
                for m in &ty.body {
                    match m {
                        TypeElement::Call(_) => {}
                        TypeElement::Constructor(_) => {}
                        TypeElement::Property(p) => {
                            keys.push(p.key.clone());
                        }
                        TypeElement::Method(m) => {
                            keys.push(m.key.clone());
                        }
                        TypeElement::Index(_) => {}
                    }
                }

                for parent in &ty.extends {
                    let parent = self.type_of_ts_entity_name(
                        span,
                        self.ctx.module_id,
                        &parent.expr,
                        parent.type_args.as_deref(),
                    )?;
                    keys.extend(self.extract_keys_as_keys(span, &parent)?);
                }

                return Ok(keys);
            }
            _ => {
                unimplemented!("extract_keys_as_keys: {:#?}", ty);
            }
        }
    }

    /// TODO(kdy1): I don't know well about TruePlusMinus currently.
    /// I have to search for it.
    fn apply_mapped_flags(
        &self,
        el: &mut TypeElement,
        optional: Option<TruePlusMinus>,
        readonly: Option<TruePlusMinus>,
    ) {
        match optional {
            Some(v) => match el {
                TypeElement::Call(_) => {}
                TypeElement::Constructor(_) => {}
                TypeElement::Property(p) => match v {
                    TruePlusMinus::True => {
                        p.optional = true;
                    }
                    TruePlusMinus::Plus => {
                        p.optional = true;
                    }
                    TruePlusMinus::Minus => {
                        p.optional = false;
                    }
                },
                TypeElement::Method(m) => match v {
                    TruePlusMinus::True => {
                        m.optional = true;
                    }
                    TruePlusMinus::Plus => {}
                    TruePlusMinus::Minus => {
                        m.optional = false;
                    }
                },
                TypeElement::Index(_) => {}
            },
            None => {}
        }

        match readonly {
            Some(v) => match el {
                TypeElement::Call(_) => {}
                TypeElement::Constructor(_) => {}
                TypeElement::Property(p) => match v {
                    TruePlusMinus::True => {
                        p.readonly = true;
                    }
                    TruePlusMinus::Plus => {
                        p.readonly = true;
                    }
                    TruePlusMinus::Minus => {
                        p.readonly = false;
                    }
                },
                TypeElement::Index(_) => {}
                TypeElement::Method(m) => match v {
                    TruePlusMinus::True => {
                        m.readonly = true;
                    }
                    TruePlusMinus::Plus => {
                        m.readonly = true;
                    }
                    TruePlusMinus::Minus => {
                        m.readonly = false;
                    }
                },
            },
            None => {}
        }
    }
}
