# Session summary — confirm destructive agent recreate shortcut

## Goal

Prevent accidental destructive agent recreation from the TUI Agent Detail shortcut. The session changed Shift+C and persistent-agent recreate fallback paths so they open the existing destructive-action confirmation modal instead of immediately issuing daemon recreate requests, and updated the modal copy to warn that recreation destroys current agent state.

## Bead(s)

- `bd-f9982a` — Add confirmation prompt for destructive agent recreation (Shift+C)

## Before state

- Failing tests: none known for the shortcut at claim time.
- Relevant metrics: Shift+C on an Agent Detail pane called `request_agent_recreate` immediately, and the persistent-agent detail fallback called `request_persistent_recreate` immediately when no backing agent was available.
- Context: button-driven `recreate` already flowed through the generic destructive-action `ConfirmModal`, but keyboard shortcut paths bypassed that safety gate.

## After state

- Failing tests: none in focused foreground validation.
- Relevant metrics: three focused TUI tests cover ordinary agent Shift+C confirmation, persistent-agent fallback confirmation, and recreate modal copy that warns about destroyed state.
- Context: Shift+C now routes through the same confirmation modal as destructive buttons. The modal defaults to No/cancel and only dispatches the daemon recreate request after explicit confirmation.

## Diff summary

- Code/content commits: `1c34a8c57` (`bd-f9982a: confirm destructive agent recreate shortcut`).
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched: `SPEC.md`, `crates/caco-tui/src/app.rs`, `crates/caco-tui/src/views/button.rs`, `.cacophony/agent/winmini-cacophony-caco-dev-wmi-2/summary/pending/summary.md`.
- Tests: +3 focused TUI tests; no tests removed or flipped.
- Validation:
  - `git diff --check`.
  - `cargo test -p caco-tui shift_c_recreate_opens_confirmation_instead_of_dispatching_bd_f9982a --lib`.
  - `cargo test -p caco-tui shift_c_persistent_recreate_opens_confirmation_without_backing_agent_bd_f9982a --lib`.
  - `cargo test -p caco-tui recreate_confirm_modal_warns_about_destroying_state_bd_f9982a --lib`.
  - `cargo build -p caco-tui`.
  - `cargo clippy -p caco-tui --lib -- -D warnings`.
- Behavioural delta: accidental Shift+C presses no longer destroy/recreate an agent immediately; the operator must explicitly confirm after seeing a state-destruction warning.

## Operator-takeaway

The high-risk Agent Detail recreate shortcut now has the same safety posture as other destructive TUI actions: confirmation first, default cancel, and explicit warning copy before the daemon receives a recreate request.
