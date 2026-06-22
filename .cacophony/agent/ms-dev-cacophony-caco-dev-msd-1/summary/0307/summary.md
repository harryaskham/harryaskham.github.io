# Session summary — bd-d4b93b sweep-health WATCH (watchdog-c follow-on)

## Goal
Close the masking edge ctrl raised post-inc2: when the daemon detection sweep silently STOPS, the
reint-wedge finding stale-drops to empty ("no wedges") — so a stalled sweep would hide real wedges
with no signal. Make the STALL itself operator-visible, distinguishing never-swept (benign) from
swept-then-stalled (a problem).

## Bead(s)
- bd-d4b93b (umbrella, in_progress, mine) — this follow-on; observe-phase (4 slices) already landed.
- Builds directly on slice-1c inc2 (the caco ops reint-wedge collector/finding).

## Before state
The inc2 collector returned routine for both a MISSING state-file (never-swept) and a STALE one
(swept-then-stalled), and dropped stale wedge findings to empty. So a daemon whose detection sweep
stopped showed "no wedges" — indistinguishable from a healthy quiet node. The sweep stall was
invisible.

## After state (sweep-health WATCH)
- The collector now reports `file_present` (true on any read state-file incl. unparseable; false on
  missing-file / runtime-dir-error) so the two cases can be told apart.
- New `reintegration_wedge_detection_health_finding` (caco ops id `reintegration.wedge_detection_health`):
  WATCH only when the state-file EXISTS but is older than SWEEP_HEALTH_STALE_SECS (45min = 3 sweeps)
  → swept-then-stalled, the ~15min sweep may have stopped. A MISSING file stays routine (never-swept,
  benign — e.g. just after the binary rolled). The generous 3-sweep threshold avoids a false positive
  across a single daemon restart gap.
- Appended alongside the wedge finding in build_ops_snapshot (standalone helper, same no-blast-radius
  pattern as inc2).
- 4 unit tests (collector marks file_present; health WATCH when present+stale; routine when
  recent/missing; missing-file never false-stalls). inc2's 5 tests unaffected (file_present additions
  validated non-regressing). caco-cli clean.
- Escalate-only / observational: a derived caco ops finding from the existing state-file; no daemon
  change, no behavior change.

## Diff summary
- crates/caco-cli/src/ops_cmd.rs: SWEEP_HEALTH_STALE_SECS const; `file_present` on the collector
  returns; reintegration_wedge_detection_health_finding helper; the append in build_ops_snapshot;
  4 unit tests.
(Final landed squash SHA: see the reintegration receipt.)

## Operator takeaway
A silently-stopped reint-wedge detection sweep is no longer invisible: `caco ops` now shows a
`reintegration.wedge_detection_health` WATCH ("detection sweep appears STALLED … may have stopped")
when the state-file ages past 3 sweeps, while a never-yet-written file stays benign routine. This
closes the "stale→empty masks a real wedge" gap. Remaining bd-d4b93b follow-ons: auto-abort/force-kill
recovery (gated on the observed false-positive rate) and the WATCH→stronger-class promotion.
