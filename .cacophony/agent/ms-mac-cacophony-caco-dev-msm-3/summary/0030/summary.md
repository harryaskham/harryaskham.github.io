# Session summary — bd-7ace15: caco summary text/JSON parity

## Goal

Fix `caco summary` text-vs-JSON inconsistencies and counter-visibility
problems reported in bd-7ace15.

## Bead(s)

- `bd-7ace15` — caco summary off-by-one + missing fields + under-reporting

## Before state

- JSON envelope had `messages_sent` only; text formatter said "N direct"
  with no JSON key consumers could find.
- Text formatter suppressed the Agents row when all four counters were
  zero, hiding under-reporting (Issues 3+4) from operators.
- Issue 1 (off-by-one errors_logged) reported in the bead but did not
  reproduce in fresh runs.

## After state

Daemon (`crates/caco-daemon/src/lib.rs`):
- `SummaryResponse` now serialises `messages_direct` alongside
  `messages_sent` (same value), with `#[serde(alias)]` so JSON
  consumers can use either name.

CLI (`crates/caco-cli/src/lib.rs`):
- Extracted `render_summary_text()` for unit testing.
- Always emits the Agents row (even all-zero) so absence stops
  hiding the deeper counter bug.
- Text reader prefers `messages_direct`, falls back to `messages_sent`
  for back-compat against older daemons.
- 3 unit tests: `agents_row_always_emitted_even_when_all_zero`,
  `messages_direct_field_preferred_when_present`,
  `messages_sent_back_compat_used_when_messages_direct_missing`.

Follow-up:
- Filed `bd-1d0e14` for Issues 3+4 (counters under-reporting at short
  windows; failed/stopped always zero) — needs feed_events vs
  agent-registry investigation, more invasive than this bead.

## Diff summary

- Commit `6044c472`
- Files: 2 (`crates/caco-daemon/src/lib.rs`, `crates/caco-cli/src/lib.rs`)
- Net: +113 / -9 lines
- Tests: 3 new (caco-cli summary_render_tests), all pass
- `cargo test-small`: 57 pass
- `cargo clippy -p caco-daemon -p caco-cli --tests`: clean

## Operator-takeaway

`caco summary` text and JSON are now in lockstep on direct-message
counts and the Agents row is always visible. The deeper "agents
counters under-report" issue is bd-1d0e14.
