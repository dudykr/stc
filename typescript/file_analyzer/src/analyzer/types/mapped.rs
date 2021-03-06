use crate::analyzer::Analyzer;
use crate::ValidationResult;
use stc_ts_ast_rnode::RTsLit;
use stc_ts_ast_rnode::RTsLitType;
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
                    if ty.normalize().type_eq(&mapped_ty) {
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
                let keys = self.get_keys(span, ty)?;
                if let Some(keys) = keys {
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
            }
            _ => {
                match m.type_param.constraint.as_deref() {
                    Some(constraint) => {
                        if let Some(keys) = self.convert_type_to_keys(span, constraint)? {
                            // TODO: Verify that m.ty does not contain key type.
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
                    }
                    None => {}
                }
            }
        }

        unimplemented!(
            "expand_mapped: {}",
            dump_type_as_string(&self.cm, &Type::Mapped(m.clone()))
        )
    }

    /// Evaluate a type and convert it to keys.
    ///
    /// Used for types like `'foo' | 'bar'` or alias of them.
    fn convert_type_to_keys(&mut self, span: Span, ty: &Type) -> ValidationResult<Option<Vec<Key>>> {
        let ty = ty.normalize();

        match ty {
            Type::Ref(..) => {
                let ty = self.expand_top_ref(span, Cow::Borrowed(ty))?;
                return self.convert_type_to_keys(span, &ty);
            }

            Type::Alias(alias) => return self.convert_type_to_keys(span, &alias.ty),

            Type::Lit(RTsLitType { lit, .. }) => match lit {
                RTsLit::BigInt(v) => return Ok(Some(vec![Key::BigInt(v.clone())])),
                RTsLit::Number(v) => return Ok(Some(vec![Key::Num(v.clone())])),
                RTsLit::Str(v) => {
                    return Ok(Some(vec![Key::Normal {
                        span: v.span,
                        sym: v.value.clone(),
                    }]))
                }
                RTsLit::Tpl(t) if t.quasis.len() == 1 => {
                    return Ok(Some(vec![Key::Normal {
                        span: t.span,
                        sym: match &t.quasis[0].cooked {
                            Some(v) => v.value.clone(),
                            _ => return Ok(None),
                        },
                    }]))
                }
                RTsLit::Bool(_) | RTsLit::Tpl(_) => return Ok(None),
            },

            Type::Union(u) => {
                let mut keys = vec![];

                for ty in &u.types {
                    let elem_keys = self.convert_type_to_keys(span, ty)?;
                    match elem_keys {
                        Some(v) => keys.extend(v),
                        None => return Ok(None),
                    }
                }

                return Ok(Some(keys));
            }

            Type::TypeLit(..) | Type::Interface(..) | Type::Class(..) | Type::ClassDef(..) => return Ok(None),

            _ => {
                slog::error!(self.logger, "unimplemented: convert_type_to_keys: {:#?}", ty);
                return Ok(None);
            }
        }
    }

    /// Evaluates `keyof` operator.
    fn get_keys(&mut self, span: Span, ty: &Type) -> ValidationResult<Option<Vec<Key>>> {
        let ty = self
            .normalize(ty, Default::default())
            .context("tried to normalize a type to get keys from it")?;
        let ty = ty.normalize();

        match ty {
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

                return Ok(Some(keys));
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
                    if let Some(parent_keys) = self.get_keys(span, &parent)? {
                        keys.extend(parent_keys);
                    }
                }

                return Ok(Some(keys));
            }
            Type::Param(..) => Ok(None),
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
