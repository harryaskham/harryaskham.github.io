# Session summary — bd-09e8df TUI rename dialog

## Goal

Add a 'Rename' agent context-menu item and text-input dialog to caco-tui so operators can set/clear an agent's `short_name` without dropping to the CLI. Follow-up to bd-34d0b8 (CLI `caco agent rename`).

## Bead(s)

- `bd-09e8df` — [bd-34d0b8 follow-up] TUI: 'Rename' agent context-menu item + text-input dialog.

## Before state

- `caco agent rename --id X --name Y` worked from the CLI but the TUI had no equivalent — operators had to leave the TUI to relabel an agent.
- Context menu for agents (`crates/caco-tui/src/views/context_menu.rs`) had Open / Stop / Pause / Resume / Restart / Recreate / Complete / Discard / Discard&Delete / Prune. No Rename.
- No `set_agent_field` on the TUI client; only narrow per-action wrappers existed.
- No text-input modal scaffolding for short single-line agent metadata edits.
- Failing tests: none (gap, not regression).

## After state

- Context menu now includes a `Rename` item between `Recreate` and `Complete`, available in any state — same shape as the CLI's `agent set`.
- New `AgentRenameDialog { agent_id, project, name: TextBuffer }` in `state/mod.rs`, opt-in field on `TuiState` initialised to `None`.
- New event variants `ActionResult::AgentRenamed { agent_id, project, new_name }` and `AgentRenameFailed { agent_id, error }` with debug impls.
- New `Client::set_agent_field(agent_id, field, value)` POSTs to `/api/v1/agents/{id}/field/set` with `x-caco-caller: tui` provenance, mirroring the CLI's `dispatch_agent_set`.
- `request_agent_rename` spawns a tokio task and forwards Result via `action_tx`.
- Dialog input handler: Enter submits (empty clears), Esc cancels, paste strips `\n`/`\r`. Mouse click outside dismisses without applying.
- Renderer `render_agent_rename_overlay` is a centered 56x7 modal in the active theme accent with hint footer `Enter=apply (empty clears) Esc=cancel`.
- Successful rename mutates the cached `Agent.short_name` in-place and triggers `rebuild_nav` so the new label appears immediately rather than waiting for the next snapshot.
- Two new tests in `state/tests.rs` cover pre-population from current name and initial-closed invariant.

## Diff summary

- Commits: `2707ac4d`.
- Files touched: `crates/caco-tui/src/{app.rs, client.rs, event.rs, state/mod.rs, state/tests.rs, views/context_menu.rs}`.
- Tests: `cargo test -p caco-tui --lib` 2795 passed (was 2793, +2 new).
- Lints: `cargo clippy -p caco-tui --all-targets -- -D warnings` clean.
- Wider blast radius: `cargo test-small` workspace — 45 + everything else passes.
- Behavioural delta: new menu item; new modal overlay; new daemon write call. No existing key bindings changed; the rename overlay only intercepts input when open.

## Operator-takeaway

The TUI input dialog scaffolding is repetitive (each new modal gets its own state field, render fn, key handler, click handler, paste handler in five separate sites). A future bead could extract a `TextInputModal` trait with a registry pattern, then port view-save / view-edit / agent-rename onto it. Out of scope here but worth noting — adding the seventh single-line modal will be cheaper if we do that first.
