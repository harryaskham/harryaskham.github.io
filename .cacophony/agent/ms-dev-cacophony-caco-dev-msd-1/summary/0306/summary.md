# Session summary — bd-b38302 watchdog-c slice-1c increment 2 (caco-cli ops finding)

## Goal
Complete slice-1c: surface the daemon-persisted reint-wedge findings (case-a phase-wedge +
case-b daemon-parented hung git op) as a PROACTIVE, operator-visible `caco ops` finding. Inc1
landed the daemon detect+persist (timestamped daemon/state/reint-wedges.json); this inc reads it
in the caco-cli ops surface.

## Bead(s)
- bd-b38302 (in_progress, mine) — slice-1c; this inc completes it (inc1 94e3358960 + inc2).
- Umbrella bd-d4b93b (in_progress, mine): cases (a) 1d1c47446d + (b) aaf5b4b489 + 1c detect/surface
  landed. Remaining = the deliberate auto-abort/force-kill recovery follow-on. bd-f6a0f5 theme.

## Before state
The daemon wrote reint-wedges.json (inc1), but nothing READ it — the wedge was not yet on the
operator-facing ops surface (ctrl: "a wedge that only logs can sit unnoticed; a wedge in caco ops
gets acted on").

## After state (inc2 — caco ops collector + finding)
- collect_reintegration_wedges reads $CACOPHONY_DIR/daemon/state/reint-wedges.json IN-PROCESS (the
  ops collectors don't use HTTP). Graceful by construction: missing / unparseable / STALE file →
  empty findings (routine), NEVER an error. Findings older than REINT_WEDGE_STALE_SECS (30min =
  2 sweeps) are dropped so caco ops never acts on a stale wedge; `age_secs`/`fresh` carry freshness.
- reintegration_wedge_finding emits a "reintegration.wedges" finding, WATCH class when wedges are
  present, with the "detected Ns ago" freshness in the summary.
- BLAST-RADIUS AVOIDANCE: the finding is appended to the findings array in build_ops_snapshot via a
  standalone helper rather than added to OpsFindingInputs — which would have rippled across all 10
  plan_findings test constructions. It's in the array before planned_actions + the concise counts,
  so its watch class is counted distinctly like any other finding.
- 5 unit tests (pure helpers: finding watch-when-present / routine-when-clear-or-stale; collector
  fresh-surfaces / stale-drops / unparseable-empty). caco-cli compiles clean; tests green.

## CATEGORIZATION CHOICE (for ctrl review)
WATCH, deliberately, for the escalate-only OBSERVE phase. A genuine wedge does block reint landing
(arguing for Blocked), but the detection has a known false-positive risk (a slow-but-progressing
legit reint, doctor's live evidence), and escalate-only exists precisely to OBSERVE the
false-positive rate before any destructive gate. Watch surfaces it for escalation without
over-claiming Blocked (which implies a deterministic recovery runbook like frozen_wal) or
OperatorChoiceRequired (a decision gate). Promoting the class once the false-positive rate is known
is a calibration follow-on (and pairs naturally with the auto-abort/force-kill recovery follow-on).

## Diff summary
- crates/caco-cli/src/ops_cmd.rs: the reintegration_wedges collector (spawn/recv/take/output wiring),
  collect_reintegration_wedges + reint_wedge_collector_from_bytes (pure) + REINT_WEDGE_STALE_SECS,
  reintegration_wedge_finding helper, the append in build_ops_snapshot, and 5 unit tests.
(Final landed squash SHA: see the reintegration receipt.)

## Operator takeaway
A detected reintegration wedge (held reint lock + frozen main + stuck phase, OR a daemon-parented
hung cacophony-state git op) now shows as a `caco ops` WATCH finding with "detected Ns ago"
freshness, gracefully silent when the daemon hasn't swept recently. This completes the escalate-only
detect+surface for both wedge classes; auto-recovery (abort/force-kill) stays the deliberate
follow-on after the false-positive rate is observed in the wild.
