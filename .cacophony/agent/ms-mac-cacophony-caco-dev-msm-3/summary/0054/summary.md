# Session summary — bd-86a4f8 perf permanent cycle 2

## Goal

Cycle 2 of workspace-view perf/telemetry permanent: measure suite
growth, file perf bead if regression > target, add at least one new
counter per cycle.

## Bead(s)

- `bd-86a4f8` — perf permanent (still claimed)
- `bd-d5b850` — NEW P3 bug filed: caco-web test suite 7.3x slowdown

## Before state

- Cycle-1 baseline: 4.9s suite wall, 8 reserved counters

## After state

- Cycle-2 measurement: 35.9s suite wall (~7.3x growth), driven by
  per-test `node` spawns in newly-landed workspace-* JS tests
- bd-d5b850 filed with three concrete remediation options (shared
  driver / pre-bundle / opt-in gate)
- 2 new counters reserved: command_palette_open_total +
  keyboard_focus_move_total (for bd-e49551 keyboard work)
- Cycle-2 entry appended to bd-86a4f8 description

## Diff summary

- Commits: 3561fe38b510
- Files: `crates/caco-web/src/tests.rs` (+5 -1)

## Operator-takeaway

Per-cycle measurement caught a real epic-scale regression operator
wouldn't otherwise notice (each bead added ~7s of test wall, nobody
measures across PRs). Filed as bd-d5b850 — fixable in one of three
patterns. Permanent's cycle cadence is justified.
