# Session summary — bd-86a4f8 workspace-view perf/telemetry permanent cycle 1

## Goal

Pre-MVP baseline cycle on workspace-view perf/telemetry permanent,
claimed during caco-ctrl burn-down round 2.

## Bead(s)

- `bd-86a4f8` — [PERMANENT] workspace-view perf + telemetry (still claimed after this cycle)

## Before state

- bd-a78749 MVP in progress; no /workspace route to measure yet
- No telemetry counter naming convention; downstream pane beads would
  invent ad-hoc names

## After state

- Reserved 8-counter namespace under `workspace.` in a source-level
  guard test `workspace_telemetry_counter_names_reserved`
- Pane/saved-view/WS/perf counters all named and documented
- Baseline measurements recorded on bead: caco-web suite 4.9s, compile
  7.4s, LoC snapshot
- Fixed two match-like-matches clippy regressions in freshly-landed
  pane-tree / responsive tests (broken-on-main for caco-web)

## Diff summary

- Commits: fa257b798f1c
- Files: `crates/caco-web/src/tests.rs` (+54 -8)

## Operator-takeaway

- Downstream pane beads (bd-a40535 terminal, bd-eaae6a chat,
  bd-b9e32e log, bd-1328dd detail, bd-fdc5f5 saved views) must emit
  counters under the reserved namespace
- Broken-on-main in caco-tui (derefed type, 9/7 args) still present;
  out of msm-3 scope per agent ruleset. Filing mental note for whoever
  owns caco-tui.
- Cycle 2 plan: once MVP lands, wire pane_open_total +
  ws_writeback_ms_p95 first.
