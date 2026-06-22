# Session summary — bd-b38302 watchdog-c slice-1c increment 1 (daemon detect+persist)

## Goal
Make the escalate-only wedge detection PROACTIVELY operator-visible (ctrl + msd-4: a `caco ops`
finding, not just reactive/log). Design (ctrl-settled): the caco ops collectors run IN-PROCESS in
caco-cli and case (a) reads the daemon's in-memory phase tracker, so detection MUST run daemon-side
and persist a state-file for the collector to read. This increment is the DAEMON side: detect both
wedge classes in the 15min sweep and persist a timestamped findings state-file. Increment 2 (the
caco-cli ops collector + plan_findings finding) follows.

## Bead(s)
- bd-b38302 (in_progress, mine) — slice-1c; this increment lands the daemon detect+persist.
- Umbrella bd-d4b93b (in_progress, mine). Cases (a) slice-1a 1d1c47446d + (b) slice-1b aaf5b4b489
  already landed escalate-only. bd-f6a0f5 (detect+surface theme).

## Before state
Both detectors existed (case a in the held-lock diagnostic only — REACTIVE; case b as a deduped
daemon-log sweep escalation). Neither was surfaced PROACTIVELY on the operator's active-remediation
surface; a wedge with no competing reint, or before log-monitor aggregation, stayed quiet.

## After state (increment 1 — daemon detection + persistence)
- `detect_and_persist_reintegration_wedges()` runs in the existing 15min direct-integration sweep:
  - case (a): for each `$CACOPHONY_DIR/daemon/checkouts/*` canonical checkout, if the reint lock is
    held by a LIVE flock holder (inspect_reintegration_lock_path .held), rev-parse HEAD and run the
    landed detect_canonical_prepare_wedge → a `prepare_wedge` finding with phase + true
    phase-duration.
  - case (b): the landed detect_daemon_parented_hung_git_ops → `hung_git_op` findings.
  - writes a TIMESTAMPED state-file `$CACOPHONY_DIR/daemon/state/reint-wedges.json`
    (`detected_at_epoch_secs` + `findings`), ALWAYS (even empty) so the collector sees freshness.
- Env-free testable variants (canonical_checkout_roots_in / reintegration_wedge_state_path_in /
  build_reint_wedge_state) so the sweep closure needs no config capture and the logic is unit-tested
  without env mutation.
- 5 unit tests (state stamps time + findings / empty still stamps / checkout-roots lists only
  project dirs / empty when missing / state path under daemon/state). slice-1a (10) + slice-1b (3)
  tests unaffected.
- Escalate-only / observational: detection + a state-file write in an existing sweep; no kill, no
  abort, no reintegration-flow change.

## Diff summary
- crates/caco-daemon/src/reintegration.rs: the checkout-roots + state-path helpers (env + env-free),
  build_reint_wedge_state, detect_and_persist_reintegration_wedges, and 5 unit tests.
- crates/caco-daemon/src/lib.rs: wire detect_and_persist_reintegration_wedges into the 15min sweep.
(Final landed squash SHA: see the reintegration receipt.)

## Operator takeaway
The daemon now records detected reint-prepare wedges (a) + daemon-parented hung git ops (b) to a
timestamped `daemon/state/reint-wedges.json` every 15min — a machine-readable wedge state operators
can already inspect. Increment 2 turns this into a proactive `caco ops` watch finding (with the
"detected Nm ago" freshness), gracefully empty when the file is missing/stale. Escalate-only; auto
recovery (abort/force-kill) stays the deliberate follow-on.
