# Session summary — Stop queue hook-event tasks falsely warning (bd-d58f5f)

## Goal

log-monitor observed a recurring non-fatal daemon WARNING on ms-mac —
`background task 'test queue hook event' exited unexpectedly; keeping daemon
alive until a listener terminates` — accumulating in crash.log during normal
test-queue operation (23→29 in ~15min, no restart). Confirm whether it's benign
per-job lifecycle churn or a real bug, and either fix the exit path or get it
out of the WARNING/crash channel so it stops accumulating.

## Bead(s)

- `bd-d58f5f` — Recurring non-fatal daemon warning on ms-mac: 'test queue hook
  event' background task exits unexpectedly during normal test-queue operation
  (P3 bug, daemon/test-queue/observer-reported).
- Adjacent: `bd-2d039d` (my prior queued-cargo CARGO_BUILD_JOBS cap),
  `bd-6f944d` (msm-3's test-thread cap), `bd-b09e13` (the spawn_oneshot_task
  precedent this fix reuses). Ownership confirmed mine by caco-ctrl after msm-3
  conceded cleanly.

## Before state

- Failing tests: none. But `crates/caco-daemon/src/lib.rs` wired both the
  test-queue and build-queue transition observers to spawn one task per
  job-state transition via `spawn_background_task` (oneshot=false). Each task
  runs a single `publish_*_job_transition_event(...).await` and completes —
  but the wrapper treats normal completion as unexpected, logging the
  'exited unexpectedly' WARNING per event and parking a `pending()` task per
  event. Result: spurious crash.log accumulation during normal test-queue use.

## After state

- Failing tests: none. caco-daemon compiles; clippy clean for the change
  (only a pre-existing unrelated `LifecycleOperationError` unused-import
  warning remains).
- Both `test queue hook event` and `build queue hook event` now spawn via
  `spawn_oneshot_task` (bd-b09e13 precedent), so normal per-event completion
  logs an info-level 'completed' line instead of the WARNING. No more crash.log
  accumulation from this path.
- Tests: +1 (`oneshot_completion_message_is_not_a_warning_bd_d58f5f`); existing
  oneshot/background wrapper tests still pass (3/3 green via queued cargo test).

## Diff summary

- Code commit: dbfc8a46a7 (final landed squash SHA from the reintegration receipt).
- Summary artefact commit: intentionally omitted.
- Files touched: crates/caco-daemon/src/lib.rs only
  (test-queue + build-queue observers → spawn_oneshot_task; extracted pure
  `background_task_completion_message(name, oneshot)` helper; +1 regression test).
- Tests: +1.
- Behavioural delta: the two queue hook-event tasks no longer emit the
  'exited unexpectedly' WARNING on normal completion. Acceptance criterion met:
  the warning stops accumulating in crash.log (root cause = wrong task wrapper),
  not merely suppressed — it's reclassified to the correct one-shot info path.

## Embedded artefacts

None. (Pure daemon-internal logging fix; validated by unit tests. The crash.log
non-accumulation is observable on ms-mac post-deploy by log-monitor, which filed
the bead.)

## Operator-takeaway

A per-event one-shot task was spawned with the long-lived-listener wrapper, so
every queued test/build job transition logged a scary-looking 'exited
unexpectedly' WARNING into crash.log even though nothing was wrong. The daemon
already had the right tool (spawn_oneshot_task, bd-b09e13) — this just routes the
two queue hook-event tasks through it. Worth noting these queue-observer tasks
also parked a pending() task per event; the oneshot path keeps that wrapper
behavior, so the parked-task pattern is unchanged by design — if per-event task
accumulation ever matters, that wrapper (not this bead) is the place to revisit.
