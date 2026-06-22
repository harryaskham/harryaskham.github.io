# Session summary — bd-7bbd79: live coverage for footer activity/context/queue indicators

## Goal

Complete the systematic snapshot-field live-coverage audit: three native-parity
footer indicators (context_percent, activity, queue_depth) were rendered by
renderPicoFooter but had zero live caco-web-observe coverage (the main scenario
clears `activity` via CompactionEnd before its assert).

## Bead(s)

- `bd-7bbd79` — live coverage for footer activity/context_percent/queue_depth indicators
- Continues the bd-b357f0 / bd-87a45b field-coverage audit.

## Before state

- Failing tests: none. The footer ctx%/activity/queue spans had no end-to-end
  test; a stale/broken one would have gone unnoticed (the widget_placements
  failure mode).

## After state

- Failing tests: none. New live subscenario serves a static snapshot with
  context_percent=42, activity="compacting…", queue_depth=3 and asserts the
  footer renders `ctx 42%`, the activity text, and `queue 3`. 2/2 clean —
  confirms all three flow through the current wasm and render correctly.
- caco-web bin 12; `--lib` 651; clippy clean.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-web/src/bin/caco-web-observe.rs` — mock + footer-indicators subscenario + eval.
- Tests: +1 live subscenario.
- Behavioural delta: test-only (coverage lock); no product change.

## Embedded artefacts

- `web/footer-run.log` — clean run incl. the footer-indicators result.

## Operator-takeaway

This locks the last untested web-read snapshot fields. With send-state (bd-87a45b)
and these footer indicators covered, essentially every snapshot field the caco-web
Pico pane reads now has an end-to-end live assertion plus the bd-b11d95 stale-wasm
guard — the field-coverage audit is complete.
