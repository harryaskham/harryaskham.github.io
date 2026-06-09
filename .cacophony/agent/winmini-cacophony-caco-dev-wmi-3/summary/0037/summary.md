# Session summary — durable lifecycle operation executor (bd-7dd5ec)

## Goal

Durable lifecycle operations were recorded but never executed: `POST
/api/v1/agents/{id}/operations` saved a `LifecycleOperation { phase: Pending }`
plus one `accepted` event and nothing ever advanced it. Any surface that created
and polled an operation (the TUI/web/mobile render-progress children) would only
ever see `Pending`. Build the missing executor that drives a recorded operation
through its phases.

## Bead(s)

- `bd-7dd5ec` — Execute and advance durable lifecycle operations (daemon
  executor) — currently record-only (P2, daemon/async-operations/blocker).
  Implemented + landed.

## Before state

- `save_lifecycle_operation(...)` was called exactly once (the create handler),
  setting `Pending` + one `accepted` event. No call site ever transitioned an
  operation to Running/Succeeded/Failed outside the pure validator
  `validate_lifecycle_phase_transition` and its unit tests.
- Failing tests: none in scope.

## After state

- New module `crates/caco-daemon/src/lifecycle_executor.rs`: a reusable driver
  `drive_lifecycle_operation(store, op_id, running_message, work)` that advances
  a recorded operation `Pending -> Running -> {Succeeded|Failed}`. It validates
  every edge with `validate_lifecycle_phase_transition`, persists each phase via
  `save_lifecycle_operation`, appends a `LifecycleOperationEvent` at each step
  (`phase_transition` for Running with an operator message; terminal
  `succeeded`/`failed`), and on failure populates `LifecycleOperation.error`
  (stable code + operator-safe message). It is a strict no-op for any
  non-`Pending` operation, so it can never double-execute work or clobber a
  terminal phase. The store lock is held only for the tiny single-row writes,
  never across the work future.
- `crates/caco-daemon/src/lib.rs`: registered `pub mod lifecycle_executor;`.
  Added `spawn_lifecycle_operation_driver(...)` and wired
  `handle_agent_lifecycle_operation_create` to, after recording the `Pending`
  operation + `accepted` event, spawn a background task that drives the
  operation to a terminal phase by invoking the real lifecycle handler and
  mapping its HTTP status to Succeeded/Failed. The operations POST stays the
  async entrypoint (returns 201 immediately; work happens in the background).
- Scope/safety: this slice executes the intents with a clean handler signature —
  `recreate`, `resume`, `stop` — which satisfies the bead's headline acceptance
  ("creating an operation for a recreate drives it through Running to a terminal
  phase with events"). The remaining intents (`create`, `discard`, `spawn`,
  `reintegrate`, `handoff`) keep their existing record-only behavior unchanged
  (the driver early-returns for them), so nothing regresses; `discard` needs the
  `Extension<CallerIdentity>` + `Query<DiscardParams>` extractors and `create`/
  `spawn` target a not-yet-existing agent, so they are deferred to a focused
  follow-up (see below).

## Diff summary

- Code commit: see the reintegration receipt for the final landed squash SHA.
- Files touched:
  - NEW `crates/caco-daemon/src/lifecycle_executor.rs`: `drive_lifecycle_operation`
    + `transition_and_event` + 3 unit tests.
  - `crates/caco-daemon/src/lib.rs`: register module; `spawn_lifecycle_operation_driver`;
    wire the operations POST to drive recreate/resume/stop.
- Tests: +3 (`drive_success_runs_pending_to_succeeded_with_events_bd_7dd5ec`,
  `drive_failure_marks_failed_with_error_bd_7dd5ec`,
  `drive_is_noop_on_non_pending_operation_bd_7dd5ec`).
- Behavioural delta: creating a recreate/resume/stop operation now drives it
  Pending -> Running -> terminal with `phase_transition` + terminal events and a
  structured error on failure, so `GET /api/v1/lifecycle-operations/{id}` and
  `.../events` reflect real progress and the render children can show live
  phases instead of a forever-Pending operation.

## Validation

- `cargo check -p caco-daemon --lib` (queued, PASSED).
- `cargo test -p caco-daemon --lib lifecycle_executor` (queued, PASSED — 3/3).
- `cargo clippy -p caco-daemon --lib` (queued, PASSED; new code is
  warning-clean). Note: `-D warnings` surfaces a single pre-existing
  redundant-closure in `crates/caco-daemon/src/agent/health.rs:3141`
  (`stored_pid.map(|pid| process_alive(pid))`, unrelated to this change and
  present since March), which the gate's non-`-D` workspace clippy tolerates.
- `git diff --check` clean.

## Follow-ups

- File a follow-up bead to wire the remaining intents (`discard` via its
  `Extension<CallerIdentity>` + `Query<DiscardParams>` extractors; `create`/
  `spawn` via their create entrypoint) into the executor, and to decide the
  auto-execute vs. opt-in semantics and the (acceptable, distinct-entrypoint)
  interaction with the legacy synchronous endpoints.

## Operator-takeaway

The operations POST is now a real async executor for recreate/resume/stop: it
runs the work in the background and advances the durable operation through its
phases with operator-facing events, unblocking the TUI/web/mobile
render-progress children (bd-3c97c1 et al). Other intents are unchanged for now;
a follow-up wires them.
