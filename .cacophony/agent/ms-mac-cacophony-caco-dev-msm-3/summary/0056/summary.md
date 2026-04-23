# Session summary — bd-e7be1a perf_events request_duration sampling

## Goal

Sample-gate Telemetry::record_perf for the request_duration metric so
it stops being 89% of perf_events insert volume per operator
directive.

## Bead(s)

- `bd-e7be1a` — perf_events request_duration dominates 89% of writes

## Before state

- helsinki audit: 32k request_duration rows/hr (88.8% of writes)
- ~9 inserts/sec sustained; ~778k rows/day per node
- daemon.db retention lowered earlier (bd-727210) but volume
  unchanged

## After state

- Slow-tail bypass: requests >= 1000ms always recorded (p99 intact)
- Otherwise 1-in-10 reservoir sample
- Both knobs env-overrideable
  (CACO_PERF_REQUEST_DURATION_SAMPLE_RATE,
  CACO_PERF_REQUEST_DURATION_ALWAYS_RECORD_MS) per the canonical
  caco escape-hatch pattern (matches bd-acfa92 / bd-220adc / bd-727210)
- Other metrics untouched (already low-volume tick-style)
- Expected per-node insert rate drops from 36k/hr to ~6.7k/hr

## Diff summary

- Commits: b8d674d919cf
- Files: `crates/caco-daemon/src/telemetry.rs` (+114 -1),
  `crates/caco-daemon/src/beads.rs` (drive-by clippy fix)
- Tests: +3

## Operator-takeaway

Sampling chosen over full roll-up because (a) preserves outlier
signal via slow-tail bypass, (b) zero schema change so no migration,
(c) env-var override means operators can tune per-node without a
deploy. If operator wants pre-aggregated minutely roll-ups (option 2
in bead suggested-fix) that becomes a follow-up bead with a fresh
table + dashboard rewrite.
