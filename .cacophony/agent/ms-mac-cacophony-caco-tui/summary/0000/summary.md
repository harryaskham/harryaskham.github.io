# Session summary — fix TUI voice attach and persistent dictation shortcuts

## Goal

Fix the operator-facing TUI voice-call regression that made Ctrl+V unusable for agent voice conversations and made the focused `&` dictation path fail from persistent agent detail panes. The goal was to make the existing voice-call affordance actually reachable again without redesigning the broader speech system.

## Bead(s)

- `bd-57aad6` — [tui/speech] Ctrl+V voice attach is swallowed by Views and & dictation fails on persistent agent detail

## Before state

- Operator report: pressing `Ctrl+V` on agent detail opened the Views popup instead of toggling voice conversation.
- Operator report: the `&` dictation path also did not work from persistent agent detail.
- Code state:
  - global plain-`v` handling in `crates/caco-tui/src/app.rs` matched before the Ctrl+V voice handler, so the views popup consumed the shortcut.
  - voice attach only handled `ContentPane::AgentDetail`, not `ContentPane::PersistentAgentDetail`.
  - `toggle_global_transcription()` mapped persistent agent detail to the persistent declaration ID instead of the live backing agent ID.

## After state

- `Ctrl+V` is now reserved for the agent-detail voice path instead of being swallowed by the Views popup.
- Voice attach now resolves targets from both regular and persistent agent detail panes via the live backing agent.
- `&` dictation now resolves persistent agent detail to the backing managed agent, and Shift+7 is also accepted for terminals that report `&` that way.
- Added regression coverage for:
  - persistent backing-agent target resolution
  - Ctrl+V bypassing the Views popup and hitting the voice path
  - persistent-agent dictation targeting the backing agent

## Diff summary

- Commits: `76618d2f`
- Files touched: `crates/caco-tui/src/app.rs`
- Tests: +3 focused regression tests
- Behavioural delta:
  - plain `v` still opens Views, but only with no modifiers.
  - `Ctrl+V` now works on both `AgentDetail` and `PersistentAgentDetail`.
  - `&` / `Shift+7` now resolve persistent detail panes to the live backing worker instead of the persistent declaration ID.
- Validation:
  - `cargo test -p caco-tui focused_agent_detail_voice_target_resolves_backing_agent_for_persistent_view --lib`
  - `cargo test -p caco-tui ctrl_v_on_agent_detail_uses_voice_path_not_views_popup --lib`
  - `cargo test -p caco-tui shift_7_dictation_on_persistent_agent_targets_backing_agent --lib`
  - `cargo build -p caco-tui`
  - `cargo test-small` was attempted but timed out in unrelated long-running tests after many passing cases; meanwhile another agent took a separate broken-on-main stack-overflow failure on main.

## Operator-takeaway

This was a real shortcut-routing bug plus a persistent-agent target-resolution bug, not operator error. The existing TUI voice-call feature was present but partially unreachable; this patch reconnects the advertised shortcuts to the correct speech paths for both normal and persistent agent detail views.
