# Session summary — empty-string validation for audio CLI

## Goal

Close the empty-string validation gap in `caco audio realtime-turn`
and `caco audio prewarm` that allowed empty `--text` / `--model`
values to hit the paid API / daemon before failing downstream.

## Bead(s)

- `bd-287646` — caco audio realtime-turn --text '' bypasses CLI
  validator and calls paid API with empty input

## Before state

- `caco audio realtime-turn --text ''` hit the gpt-realtime-1.5 API,
  failed with 'daemon realtime response missing data.audio'.
- `caco audio prewarm --model ''` logged 'Prewarming model via daemon'
  before daemon-side rejection.

## After state

- Both commands reject empty values client-side with clear error.
- 2 regression tests added, cargo test-small 256/256.

## Diff summary

- Commit: `881af2e92`
- Files: `crates/caco-cli/src/lib.rs` (+51)
- Tests: +2

## Operator-takeaway

Quick footgun closure — empty strings from shell-quote escapes now
fail fast with an actionable message instead of burning an API call.
