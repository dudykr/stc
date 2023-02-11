use std::borrow::Cow;

use stc_ts_errors::ErrorKind;
use stc_ts_types::{TsExpr, Type, TypeElement, TypeLit};
use stc_utils::{cache::Freeze, dev_span};
use swc_common::{Span, TypeEq, DUMMY_SP};

use crate::{
    analyzer::{assign::AssignOpts, Analyzer},
    VResult,
};

impl Analyzer<'_, '_> {
    pub(super) fn report_error_for_wrong_interface_inheritance(&mut self, span: Span, body: &[TypeElement], parent: &[TsExpr]) {
        let _tracing = dev_span!("report_error_for_wrong_interface_inheritance");

        if self.config.is_builtin || self.config.is_dts {
            return;
        }
        if body.is_empty() {
            return;
        }

        for p in parent.iter() {
            let res: VResult<()> = try {
                let parent = self.type_of_ts_entity_name(span, &p.expr, p.type_args.as_deref())?;
                let parent = self.normalize(None, Cow::Owned(parent), Default::default())?.freezed();

                if matches!(
                    parent.normalize(),
                    Type::Mapped(..)
                        | Type::Tuple(..)
                        | Type::Function(..)
                        | Type::Constructor(..)
                        | Type::Array(..)
                        | Type::Enum(..)
                        | Type::Namespace(..)
                        | Type::Module(..)
                ) {
                    continue;
                }

                self.assign_with_opts(
                    &mut Default::default(),
                    &parent,
                    &Type::TypeLit(TypeLit {
                        span: DUMMY_SP,
                        members: body.to_vec(),
                        metadata: Default::default(),
                        tracker: Default::default(),
                    }),
                    AssignOpts {
                        span,
                        allow_unknown_rhs: Some(true),
                        allow_missing_fields: true,
                        allow_assignment_of_param: true,
                        skip_call_and_constructor_elem: true,
                        treat_array_as_interfaces: true,
                        ..Default::default()
                    },
                )?;
            };

            if let Err(err) = res {
                self.storage
                    .report(ErrorKind::InvalidInterfaceInheritance { span, cause: box err }.into());
                return;
            }
        }
    }

    pub(crate) fn report_error_for_conflicting_parents(&mut self, span: Span, parent: &[TsExpr]) {
        let _tracing = dev_span!("report_error_for_conflicting_parents");

        if self.config.is_builtin || self.config.is_dts {
            return;
        }

        for (i, p1) in parent.iter().enumerate() {
            let res: VResult<()> = try {
                let p1_type = self.type_of_ts_entity_name(span, &p1.expr, p1.type_args.as_deref())?.freezed();

                for (j, p2) in parent.iter().enumerate() {
                    if i <= j {
                        continue;
                    }
                    // Declaration merging
                    if p1.type_eq(p2) {
                        continue;
                    }

                    let p2 = self.type_of_ts_entity_name(span, &p2.expr, p2.type_args.as_deref())?.freezed();

                    if let Err(err) = self.assign_with_opts(
                        &mut Default::default(),
                        &p1_type,
                        &p2,
                        AssignOpts {
                            span,
                            // required because interface can extend classes
                            use_missing_fields_for_class: true,
                            allow_unknown_rhs: Some(true),
                            ..Default::default()
                        },
                    ) {
                        match &*err {
                            ErrorKind::MissingFields { .. } => {}
                            ErrorKind::Errors { errors, .. }
                                if errors.iter().all(|err| matches!(&**err, ErrorKind::MissingFields { .. })) => {}
                            ErrorKind::ObjectAssignFailed { .. } => {}
                            _ => self.storage.report(err.convert(|err| ErrorKind::InterfaceNotCompatible { span })),
                        }
                    }
                }
            };
        }
    }
}
