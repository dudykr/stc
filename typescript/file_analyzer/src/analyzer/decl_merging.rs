use super::assign::AssignOpts;
use super::util::ResultExt;
use super::Analyzer;
use crate::ValidationResult;
use fxhash::FxHashMap;
use stc_ts_errors::debug::dump_type_as_string;
use stc_ts_errors::DebugExt;
use stc_ts_errors::Error;
use stc_ts_types::ClassDef;
use stc_ts_types::ClassMember;
use stc_ts_types::ClassProperty;
use stc_ts_types::Id;
use stc_ts_types::Interface;
use stc_ts_types::Method;
use stc_ts_types::Type;
use stc_ts_types::TypeElement;
use stc_ts_types::TypeParam;
use swc_common::Span;
use swc_common::Spanned;

impl Analyzer<'_, '_> {
    fn type_element_to_class_member(&mut self, el: &TypeElement) -> ValidationResult<Option<ClassMember>> {
        match el {
            TypeElement::Call(_) => Ok(None),
            TypeElement::Constructor(c) => Ok(Some(ClassMember::Constructor(c.clone()))),
            TypeElement::Property(p) => Ok(Some(ClassMember::Property(ClassProperty {
                span: p.span,
                key: p.key.clone(),
                value: p.type_ann.clone(),
                is_static: false,
                accessibility: p.accessibility,
                is_abstract: false,
                is_optional: p.optional,
                readonly: p.readonly,
                definite: false,
            }))),
            TypeElement::Method(m) => Ok(Some(ClassMember::Method(Method {
                span: m.span,
                accessibility: m.accessibility,
                key: m.key.clone(),
                is_static: false,
                is_abstract: false,
                is_optional: m.optional,
                type_params: m.type_params.clone(),
                params: m.params.clone(),
                ret_ty: m.ret_ty.clone().unwrap_or_else(|| box Type::any(m.span)),
                kind: swc_ecma_ast::MethodKind::Method,
            }))),
            TypeElement::Index(i) => Ok(Some(ClassMember::IndexSignature(i.clone()))),
        }
    }

    fn merge_from_to(&mut self, span: Span, a: Type, b: Type) -> ValidationResult<Option<Type>> {
        if self.is_builtin {
            return Ok(None);
        }

        debug_assert!(a.is_clone_cheap());
        debug_assert!(b.is_clone_cheap());

        match (a.normalize(), b.normalize()) {
            (Type::ClassDef(a), Type::Interface(bi)) => {
                // TOOD: Handle the number of type parameters.
                let mut type_params = FxHashMap::default();
                if let Some(b_tps) = &bi.type_params {
                    if let Some(a_tp) = &a.type_params {
                        for (idx, b_tp) in b_tps.params.iter().enumerate() {
                            type_params.insert(
                                b_tp.name.clone(),
                                Type::Param(TypeParam {
                                    span: a_tp.span,
                                    name: a_tp.params[idx].name.clone(),
                                    constraint: None,
                                    default: None,
                                }),
                            );
                        }
                    }
                }
                let b = self.expand_type_params(&type_params, b)?;

                let mut new_members = a.body.clone();

                let b = self
                    .type_to_type_lit(span, &b)
                    .context("tried to convert an interface to a type literal to merge with a class definition")?;
                if let Some(b) = b {
                    for el in &b.members {
                        new_members.extend(self.type_element_to_class_member(el)?);
                    }

                    return Ok(Some(Type::ClassDef(ClassDef {
                        body: new_members,
                        ..a.clone()
                    })));
                }
            }

            (Type::Interface(a), Type::Interface(bi)) => {
                // TOOD: Handle the number of type parameters.
                let mut type_params = FxHashMap::default();
                if let Some(b_tps) = &bi.type_params {
                    if let Some(a_tp) = &a.type_params {
                        for (idx, b_tp) in b_tps.params.iter().enumerate() {
                            type_params.insert(
                                b_tp.name.clone(),
                                Type::Param(TypeParam {
                                    span: a_tp.span,
                                    name: a_tp.params[idx].name.clone(),
                                    constraint: None,
                                    default: None,
                                }),
                            );
                        }
                    }
                }
                let b = self.expand_type_params(&type_params, b)?;

                let mut new_members = a.body.clone();

                // Convert to a type literal first.
                if let Some(b) = self
                    .type_to_type_lit(span, &b)
                    .context("tried to convert an interface to a type literal to merge with another interface")?
                {
                    new_members.extend(b.into_owned().members);

                    return Ok(Some(Type::Interface(Interface {
                        body: new_members,
                        ..a.clone()
                    })));
                }
            }

            _ => {}
        }

        Ok(None)
    }

    fn validate_fn_overloads(&mut self, span: Span, orig: &Type, new: &Type) -> ValidationResult<()> {
        for orig in orig.iter_union().flat_map(|ty| ty.iter_union()) {
            match orig.normalize() {
                Type::Function(..) => {
                    let res: ValidationResult<_> = try {
                        self.assign_with_opts(
                            AssignOpts {
                                span,
                                ..Default::default()
                            },
                            &new,
                            &orig,
                        )
                        .context("tried to validate signatures of overloaded functions")?;
                    };

                    res.convert_err(|err| Error::ImcompatibleFnOverload { span: err.span() })
                        .report(&mut self.storage);
                }
                _ => {}
            }
        }

        Ok(())
    }

    fn merge_types(&mut self, span: Span, orig: Type, new: Type) -> ValidationResult<Type> {
        debug_assert!(orig.is_clone_cheap());
        debug_assert!(new.is_clone_cheap());

        if let Some(new_ty) = self.merge_from_to(span, orig.clone(), new.clone())? {
            return Ok(new_ty);
        }
        if let Some(new_ty) = self.merge_from_to(span, new.clone(), orig)? {
            return Ok(new_ty);
        }

        Ok(new)
    }

    /// Note: This method also validates function overloads.
    pub(crate) fn merge_decl_with_name(&mut self, name: Id, new: Type) -> ValidationResult<(Type, bool)> {
        let orig = self.find_type(self.ctx.module_id, &name)?;
        let mut orig = match orig {
            Some(v) => v,
            None => return Ok((new, false)),
        };

        let orig = orig.next().unwrap().into_owned();

        self.validate_with(|a| a.validate_fn_overloads(new.span(), &orig, &new));

        let new = self.merge_types(new.span(), orig, new)?;
        slog::info!(
            self.logger,
            "Merging declaration {} with type {}",
            name,
            dump_type_as_string(&self.cm, &new)
        );

        Ok((new, true))
    }
}
