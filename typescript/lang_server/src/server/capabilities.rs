use lsp_types::CallHierarchyOptions;
use lsp_types::CallHierarchyServerCapability;
use lsp_types::ClientCapabilities;
use lsp_types::CodeActionKind;
use lsp_types::CodeActionOptions;
use lsp_types::CodeActionProviderCapability;
use lsp_types::CompletionOptions;
use lsp_types::DeclarationCapability;
use lsp_types::DeclarationOptions;
use lsp_types::DefinitionOptions;
use lsp_types::DocumentLinkOptions;
use lsp_types::DocumentSymbolOptions;
use lsp_types::ExecuteCommandOptions;
use lsp_types::HoverOptions;
use lsp_types::HoverProviderCapability;
use lsp_types::ImplementationProviderCapability;
use lsp_types::MonikerOptions;
use lsp_types::MonikerRegistrationOptions;
use lsp_types::MonikerServerCapabilities;
use lsp_types::OneOf;
use lsp_types::ReferencesOptions;
use lsp_types::SaveOptions;
use lsp_types::ServerCapabilities;
use lsp_types::SignatureHelpOptions;
use lsp_types::StaticTextDocumentRegistrationOptions;
use lsp_types::TextDocumentRegistrationOptions;
use lsp_types::TextDocumentSyncCapability;
use lsp_types::TextDocumentSyncKind;
use lsp_types::TextDocumentSyncOptions;
use lsp_types::TextDocumentSyncSaveOptions;
use lsp_types::TypeDefinitionProviderCapability;
use lsp_types::WorkDoneProgressOptions;
use lsp_types::WorkspaceFileOperationsServerCapabilities;
use lsp_types::WorkspaceFoldersServerCapabilities;
use lsp_types::WorkspaceServerCapabilities;
use lsp_types::WorkspaceSymbolOptions;

pub fn server_capabilities(_capabilities: ClientCapabilities) -> ServerCapabilities {
    ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: Some(false),
                change: Some(TextDocumentSyncKind::Full),
                will_save: Some(true),
                will_save_wait_until: Some(true),
                save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                    include_text: Some(false),
                })),
            },
        )),
        selection_range_provider: None,
        hover_provider: Some(HoverProviderCapability::Options(HoverOptions {
            work_done_progress_options: WorkDoneProgressOptions {
                work_done_progress: Some(true),
            },
        })),
        completion_provider: Some(CompletionOptions {
            resolve_provider: None,
            trigger_characters: None,
            work_done_progress_options: WorkDoneProgressOptions {
                work_done_progress: Some(true),
            },
        }),
        signature_help_provider: Some(SignatureHelpOptions {
            trigger_characters: Some(vec![":".to_string()]),
            retrigger_characters: None,
            work_done_progress_options: WorkDoneProgressOptions {
                work_done_progress: Some(true),
            },
        }),
        definition_provider: Some(OneOf::Right(DefinitionOptions {
            work_done_progress_options: WorkDoneProgressOptions {
                work_done_progress: Some(true),
            },
        })),
        type_definition_provider: Some(TypeDefinitionProviderCapability::Options(
            StaticTextDocumentRegistrationOptions {
                id: Some("stc-ts-lang-server".to_string()),
                document_selector: None,
            },
        )),
        implementation_provider: Some(ImplementationProviderCapability::Options(
            StaticTextDocumentRegistrationOptions {
                id: Some("stc-ts-lang-server".to_string()),
                document_selector: None,
            },
        )),
        references_provider: Some(OneOf::Right(ReferencesOptions {
            work_done_progress_options: WorkDoneProgressOptions {
                work_done_progress: Some(true),
            },
        })),
        document_highlight_provider: None,
        document_symbol_provider: Some(OneOf::Right(DocumentSymbolOptions {
            work_done_progress_options: WorkDoneProgressOptions {
                work_done_progress: Some(true),
            },
        })),
        workspace_symbol_provider: Some(OneOf::Right(WorkspaceSymbolOptions {
            work_done_progress_options: WorkDoneProgressOptions {
                work_done_progress: Some(true),
            },
        })),
        code_action_provider: Some(CodeActionProviderCapability::Options(CodeActionOptions {
            work_done_progress_options: WorkDoneProgressOptions {
                work_done_progress: Some(true),
            },
            resolve_provider: None,
            code_action_kinds: Some(vec![CodeActionKind::QUICKFIX]),
        })),
        code_lens_provider: None,
        document_formatting_provider: None,
        document_range_formatting_provider: None,
        document_on_type_formatting_provider: None,
        rename_provider: None,
        document_link_provider: Some(DocumentLinkOptions {
            resolve_provider: None,
            work_done_progress_options: WorkDoneProgressOptions {
                work_done_progress: Some(true),
            },
        }),
        color_provider: None,
        folding_range_provider: None,
        declaration_provider: Some(DeclarationCapability::Options(DeclarationOptions {
            work_done_progress_options: WorkDoneProgressOptions {
                work_done_progress: Some(true),
            },
        })),
        execute_command_provider: Some(ExecuteCommandOptions {
            commands: vec!["Run".to_string()],
            work_done_progress_options: WorkDoneProgressOptions {
                work_done_progress: Some(true),
            },
        }),
        workspace: Some(WorkspaceServerCapabilities {
            workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                supported: Some(true),
                change_notifications: Some(OneOf::Left(true)),
            }),
            file_operations: Some(WorkspaceFileOperationsServerCapabilities {
                did_create: None,
                will_create: None,
                did_rename: None,
                will_rename: None,
                did_delete: None,
                will_delete: None,
            }),
        }),
        call_hierarchy_provider: Some(CallHierarchyServerCapability::Options(
            CallHierarchyOptions {
                work_done_progress_options: WorkDoneProgressOptions {
                    work_done_progress: Some(true),
                },
            },
        )),
        semantic_tokens_provider: None,
        moniker_provider: Some(OneOf::Right(
            MonikerServerCapabilities::RegistrationOptions(MonikerRegistrationOptions {
                text_document_registration_options: TextDocumentRegistrationOptions {
                    document_selector: None,
                },
                moniker_options: MonikerOptions {
                    work_done_progress_options: WorkDoneProgressOptions {
                        work_done_progress: Some(true),
                    },
                },
            }),
        )),
        linked_editing_range_provider: None,
        experimental: None,
    }
}
