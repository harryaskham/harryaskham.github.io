# Session summary — TUI persistent recovery row: Resume affordance (bd-598ca0)

## Goal

Implement the highest-value slice of bd-598ca0 ("Add missing agent control
buttons to TUI") using aur-2's audit (bd-648239): the persistent recovery
action row (failed/stopped persistent agents with no live backing agent) was
missing the operator-expected Resume affordance and mislabeled its start action
as "Restart".

## Bead(s)

- `bd-598ca0` — Add missing agent control buttons to TUI (implemented this slice).
- Follow-up filed: `bd-f3482b` — Add persistent-agent discard/stop endpoint so the
  recovery row can also offer Discard (needs a new caco-daemon endpoint; gate-capable).

## Before state

- `persistent_recovery_action_buttons` (button.rs ~L1083) offered only Restart +
  Recreate. The "Restart" was semantically a start/resume — it dispatched
  `persistent_start` — diverging from the main `agent_action_buttons` model where
  failed/stopped agents show "Resume". Operators viewing a stopped persistent
  could not click a Resume affordance (the 2026-06-01 operator symptom).

## After state

- Recovery row now shows **Resume** (action id `resume`) + Recreate for
  manually-startable (failed/stopped/pending-non-autostart) persistents, matching
  the main agent action bar (W1 resolved; M1 Resume delivered).
- Builder params renamed `is_restartable`->`is_resumable`,
  `restart_in_flight`->`resume_in_flight`. Recovery dispatch in app.rs accepts
  both `resume` and legacy `restart` action ids and routes to
  `request_persistent_start`. Keyboard start path (already calling
  `request_persistent_start`) is unaffected.
- M3 (Restart-vs-Resume rationale) documented in the builder doc-comment.
- Discard intentionally NOT added: no daemon endpoint exists to discard/stop a
  no-backing-agent persistent, so it would be a dead button (audit forbids dead
  buttons). Tracked in bd-f3482b.

## Diff summary

- Code commits: pending final squash SHA from the reintegration receipt.
- Files: `crates/caco-tui/src/views/button.rs` (builder relabel + doc + 3 new
  unit tests), `crates/caco-tui/src/views/agent_detail.rs` (call site rename +
  3 updated render tests now asserting Resume label / `resume` hit-map key),
  `crates/caco-tui/src/app.rs` (recovery dispatch accepts `resume`).
- Tests: 3 new + 3 updated, all passing (44 persistent-related caco-tui tests
  green); `cargo clippy -p caco-tui --lib` clean.
- caco-tui-only change (no caco-daemon), so it lands within aurora's current
  300s reintegration gate, unlike daemon work which is gate-blocked here.

## Operator-takeaway

A failed or stopped persistent agent's TUI detail page now shows a **Resume**
button (previously a confusingly mislabeled "Restart"), so you can bring a
dead-but-declared persistent back up directly from the pane. Discard on that
recovery page still needs a daemon endpoint first (bd-f3482b).
