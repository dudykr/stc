use super::InferData;
use crate::analyzer::Analyzer;
use crate::ValidationResult;
use stc_ts_types::Interface;
use stc_ts_types::TypeElement;
use stc_ts_types::TypeLit;
use swc_common::Spanned;
use swc_common::TypeEq;

impl Analyzer<'_, '_> {
    pub(super) fn infer_type_using_interface_and_interface(
        &mut self,
        inferred: &mut InferData,
        param: &Interface,
        arg: &Interface,
    ) -> ValidationResult<()> {
        dbg!(&param.name, &arg.name);
        self.infer_type_using_type_elements_and_type_elements(inferred, &param.body, &arg.body)?;

        // TODO: Handle parents.

        Ok(())
    }

    /// Compare fields.
    pub(super) fn infer_type_lit(
        &mut self,
        inferred: &mut InferData,
        param: &TypeLit,
        arg: &TypeLit,
    ) -> ValidationResult<()> {
        self.infer_type_using_type_elements_and_type_elements(inferred, &param.members, &arg.members)
    }

    fn infer_type_using_type_elements_and_type_elements(
        &mut self,
        inferred: &mut InferData,
        param: &[TypeElement],
        arg: &[TypeElement],
    ) -> ValidationResult<()> {
        for p in param {
            for a in arg {
                //

                match p {
                    TypeElement::Property(p) => match a {
                        TypeElement::Property(a) => {
                            if p.key.type_eq(&a.key) {
                                if let Some(pt) = &p.type_ann {
                                    if let Some(at) = &a.type_ann {
                                        self.infer_type(inferred, pt, at)?;
                                    } else {
                                        dbg!((&p, &a));
                                    }
                                } else {
                                    dbg!((&p, &a));
                                }
                            }
                        }
                        _ => {}
                    },
                    TypeElement::Index(param) => match a {
                        // TypeElement::Property(arg) => {
                        //     if param.params.len() != 1 {
                        //         unimplemented!("handling of IndexSignature with zero / multiple parameters");
                        //     }

                        //     if let Some(p_type_ann) = &param.type_ann {
                        //         if let Some(a_type_ann) = &arg.type_ann {
                        //             self.infer_type(inferred, p_type_ann, a_type_ann)?;
                        //         }
                        //     }
                        // }
                        TypeElement::Index(arg) => {
                            if param.params.type_eq(&arg.params) {
                                if let Some(pt) = &param.type_ann {
                                    if let Some(at) = &arg.type_ann {
                                        self.infer_type(inferred, pt, at)?;
                                    }
                                } else {
                                    dbg!((&param, &arg));
                                }
                            } else {
                                dbg!((&param, &arg));
                            }
                        }
                        _ => {}
                    },

                    TypeElement::Method(p) => match a {
                        TypeElement::Method(a) => {
                            if self.assign(&p.key.ty(), &a.key.ty(), a.key.span()).is_ok() {
                                self.infer_type_of_fn_params(inferred, &p.params, &a.params)?;

                                if let Some(p_ret) = &p.ret_ty {
                                    if let Some(a_ret) = &a.ret_ty {
                                        self.infer_type(inferred, &p_ret, &a_ret)?;
                                    }
                                }
                            }
                        }
                        _ => {}
                    },

                    TypeElement::Constructor(..) => {
                        // TODO
                    }
                    _ => unimplemented!("TypeElement({:#?}) in type literal", p),
                }
            }
        }

        Ok(())
    }
}
