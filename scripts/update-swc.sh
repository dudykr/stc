#!/usr/bin/env bash
set -eux

cargo upgrade swc_atoms \
    swc_common \
    swc_visit \
    swc_visit_macros \
    swc_emca_ast \
    swc_ecma_codegen \
    swc_ecma_codegen_macros \
    swc_ecma_parser \
    swc_ecma_parser_macros \
    swc_ecma_transforms \
    swc_ecma_transforms_macros \
    swc_ecma_utils \
    swc_ecma_visit --workspace