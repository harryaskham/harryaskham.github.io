# Session summary — bd-b87804 diagnostic-enrichment slice (reint-lock holder classification)

## Goal
ctrl-approved non-destructive slice of bd-b87804 (watchdog-c auto-recovery): give the reint-prepare
wedge finding LIVE-DIAGNOSIS observability — WHO holds the lock (PID) + which class (alive-stuck vs
provably-dead) — so a wedge is self-evident, feeding both bd-b87804's future gated auto-abort AND
the conditional bd-a22494 provably-dead reap. Read-side only; the destructive abort stays gated.

## Bead(s)
- bd-b87804 (in_progress, mine). This slice lands the diagnostic enrichment; the destructive
  auto-abort stays GATED on the observe-phase false-positive rate.
- Reuses my bd-c4665b same-process liveness test (start-time vs acquired_at). Cross-links bd-d4b93b
  (detection umbrella), bd-a22494 (the dead-reap residual, co-own with bd-c4665b).

## Context (ctrl live diagnosis)
The June-19 23:00 wedge was a LONG-HELD lock by a LIVE holder under direct mode (gate holds the lock;
PR mode isn't default) = bd-b87804's alive-stuck target — a real observe datapoint. The current
leftover lock is a benign held=false (no holder), confirming flock-releases-on-death, so the
provably-dead reap is genuinely CONDITIONAL.

## After state (daemon-side enrichment; no caco-cli change)
- ReintLockHolderClass {AliveStuck, ProvablyDead, Unknown} + as_str.
- Pure `reint_lock_holder_class(holder_pid, same_live_process)`: None→Unknown; Some+same-live→
  AliveStuck; Some+!same-live→ProvablyDead. ProvablyDead is only the CONFIDENT case (gone/recycled
  pid via bd-c4665b); AliveStuck is the conservative default (so a live holder is never mislabeled).
- `reint_lock_holder_pid` (lsof -t the lock file, best-effort, never invents a PID) +
  `classify_reint_lock_holder` (reuses crate::merge_queue::runner_lock_holder_alive, now pub(crate)).
- detect_and_persist_reintegration_wedges: the prepare_wedge finding now carries `holder_pid` +
  `holder_class` (+ in detail), flowing through the existing caco ops finding's findings array (no
  caco-cli change needed).
- 1 unit test for the pure mapping. caco-daemon compiles clean.
- Read-only / observational: lsof + a same-process liveness read; no destructive action, no
  reint-flow change. Both tightens the watchdog (a recycled pid can't masquerade as an alive-stuck
  wedge) and provides the live-diagnosis WHO+class.

## Diff summary
- crates/caco-daemon/src/merge_queue.rs: runner_lock_holder_alive -> pub(crate) (reuse the
  bd-c4665b same-process check).
- crates/caco-daemon/src/reintegration.rs: ReintLockHolderClass + reint_lock_holder_class (pure) +
  reint_lock_holder_pid (lsof) + classify_reint_lock_holder; holder_pid/holder_class on the
  prepare_wedge finding; 1 unit test.
(Final landed squash SHA: see the reintegration receipt.)

## Operator takeaway
A detected reint-prepare wedge now shows its holder PID + class (alive-stuck vs provably-dead) in the
caco ops finding — the live-diagnosis observability the reint-path residual needed, with zero
destructive action. The destructive auto-abort (bd-b87804) stays gated on the observe-phase data this
classification now feeds; the conditional provably-dead reap (bd-a22494) is a fresh-worker follow-on
co-owned with bd-c4665b.
