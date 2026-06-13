# bd-8cc120 — brace/bracket keys swallowed in choice-reply textbox (partial fix + clarification)

## Bead
bd-8cc120 (input/keybinding/tui, P2; filer harryaskham). `[`/`]`/`{`/`}` cannot be typed in textboxes (swallowed) + reportedly don't work for agent subpanel tab nav. AC: (1) `[` works for agent subpanel tab nav, (2) `]` works for agent subpanel tab nav, (3) both typeable in textbox input fields, (4) remove the intercepting code.

## Investigation
Mapped all `[`/`]`/`{`/`}` handlers in caco-tui handle_key:
- `[` (workspace prev-tab) / `]` (workspace next-tab) at app.rs ~11617/11635, guarded `!nav_focused && !bottom_panel_focused && show_bar()`.
- `{` / `}` (agent-detail inner-tab cycle) at ~11686/11720, guarded `!nav_focused && !bottom_panel_focused`; plus `handle_agent_detail_inner_tab_shortcut` (10052, AgentDetail panes only).
Verified the COMMON textbox states already capture these keys correctly (early-return before the tab handlers): chat/agent-chat composers, bead_edit/view_edit dialogs, bead_search, and the content-pane searches via `is_content_text_input_active` (config/logs/summaries/source/log-tag).

CONFIRMED GAP (criteria 3+4): the inbox composer early return (app.rs ~11329) guarded ONLY on `inbox_composing`, but `handle_inbox_compose_key` handles BOTH `inbox_composing` and `inbox_choice_composing`. So when composing a freeform CHOICE reply (`inbox_choice_composing=true`, `inbox_composing=false`), `[`/`]`/`{`/`}` (and other plain-char shortcuts like `b`/`f`) fell through to the global tab handlers and were swallowed instead of typed.

## Fix
One-line guard extension: `if self.state.inbox_composing || self.state.inbox_choice_composing`. + regression test `inbox_choice_compose_captures_brace_and_bracket_keys_bd_8cc120` (asserts `[`/`]`/`{`/`}` are typed into the reply field and the composer stays open).

## Validation (daemon test queue)
- `cargo clippy -p caco-tui --lib` (tj-a7a0df04): exit 0, clean.
- `cargo test -p caco-tui --lib inbox_choice_compose_captures` (tj-e844c843): 1 passed.
- rustfmt-clean; git diff --check clean.

## Remaining / clarification (messaged harryaskham; bead kept in_progress)
This fixes criteria 3+4 for the confirmed inbox choice-reply textbox gap. NOT yet addressed:
- AC 1+2 ("`[`/`]` for agent subpanel tab navigation"): currently `[`/`]` are WORKSPACE tab cycling and `{`/`}` are the agent-subpanel inner-tab cycling. Re-mapping `[`/`]` to agent-subpanel nav would break workspace tab cycling, so it needs operator confirmation of intent before changing.
- Whether the operator's "can't type in textboxes" repro was the choice-reply field (now fixed) or a different specific view — asked for the exact view/textbox so any remaining gap can be pinned.

## Diff
See reintegration receipt for the landed squash SHA.
