# Session summary — caco bd triage --interactive draft loop

## Goal

Add the interactive UX layer described in bd-eef036 on top of the existing
non-interactive `caco bd triage --next/--promote/--discard/--defer`
primitives, so an operator can sit at a terminal and triage the draft
backlog one bead at a time without scripting glue.

## Bead(s)

- `bd-eef036` — [bd-2e2338 follow-up] caco bd triage --interactive:
  promote/discard/merge-into/defer/label loop on the draft pool

## Before state

- `caco bd triage` only exposed read-only `--next` and per-bead-id
  `--promote / --discard / --defer` (each requiring `--bead-id`),
  meaning operators had to script around `caco bd triage --next --json |
  jq -r ...` to get an interactive feel.
- No merge-into action; no general add-label action.
- No session cap.
- Failing tests: none.

## After state

- `caco bd triage --interactive` enters a per-bead loop:
  P (promote, optional priority bump), D (discard with reason), M
  (merge-into another bead, source description appended to target,
  source closed with audit trail), F (defer label
  `deferred-YYYY-MM-DD`), L (add a label, preserving existing labels by
  GET-then-PUT), S (skip), Q (quit).
- `--max N` (default 50) caps a session.
- `--type` / `--priority` filters scope the draft queue.
- The draft list is re-fetched each iteration so concurrent peer triage
  is observed.
- `--interactive` + `--json` is rejected with `invalid_argument`.
- Two new unit tests:
  `bd_triage_args_includes_interactive_and_max`,
  `triage_session_summary_total_sums_all_actions`.
- `cargo test-small` 57/57 PASS, `cargo clippy -p caco-cli --lib --tests`
  clean, `cargo build` clean.

## Diff summary

- Commit: 1c14e280
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: +2 / -0 / flipped 0
- Behavioural delta: new interactive triage mode; existing modes
  unchanged.

## Operator-takeaway

This bead is the second slice of the bd-2e2338 triage epic — read-only
`--next` shipped first, scripted single-bead actions second, now an
interactive loop. The L (label) action is intentionally GET-then-PUT
rather than a delta-style PATCH so existing provenance labels
(`discovered-via-*`) are preserved across triage; if a future bead adds
server-side label-set semantics that change should be a one-line swap.
The M (merge-into) action is end-state-only — it appends the source
description into the target and closes the source; it does not try to
move metadata, dependencies, or assignees, because in practice draft
beads being merged carry no such state.

Out of scope but called out in the original bead: dup-detection by
title-similarity for auto-suggested merges, sort by age/type/priority,
batch heuristics. File follow-ups when an operator actually hits the
need.
