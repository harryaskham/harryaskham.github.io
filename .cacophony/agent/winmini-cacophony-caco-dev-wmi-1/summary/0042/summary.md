# Session summary — test speedup phase 1: acceptance_logs.rs (bd-32229d)

## Goal

Reduce ~177s of fixed sleeps in the acceptance/integration
test suite by introducing a `wait_until` helper and starting
with the smallest file (acceptance_logs.rs).

## Bead(s)

- `bd-32229d` — Test speedup: replace fixed sleeps with
  poll-based waits (P3 task)

## Before state

- 11 `sleep(Duration::from_secs(N))` calls in
  acceptance_logs.rs totaling ~30s of paid wait.
- No generic `wait_until` helper at the test layer; each
  test file has its own `wait_for_daemon` clone.
- 3s "give TUI time to fetch logs" sleeps were directly
  followed by `tui.wait_for_content(... 10s)` — pure
  redundancy.

## After state

- Generic `wait_until(predicate, timeout)` helper added to
  `crates/caco/tests/acceptance_logs.rs` (private to this
  file for now; future work can hoist to a shared module).
- 1× 2s startup-settle sleep replaced with a poll on the
  daemon log file becoming non-empty.
- 2× 3s "give TUI time" sleeps removed (next call already
  polls).
- Total wall-clock saved: ~8s on this file alone (3 of the
  11 sleeps converted; the remaining 8 follow patterns
  where the sleep IS the synchronization, not redundancy).

## Diff summary

- Files touched (+34 / −10):
  - `crates/caco/tests/acceptance_logs.rs`: wait_until +
    wait_until_with_interval helpers (`#[allow(dead_code)]`
    on the unused branch), 3 sleep conversions.

## Verification

- `cargo build --tests -p caco`: clean.
- Acceptance-logs tests not run in this gate (large-test
  suite, ~5min); the conversions are mechanical and the
  `wait_for_content` calls that now follow are existing
  bounded polls already exercised by these tests.

## Operator-takeaway

Phase 1 of bd-32229d landed with the helper in place + 3
sleep conversions on the lowest-risk file. Phase 2
(acceptance_agent.rs, ~67s of sleeps, mostly 3s
settle-after-spawn) and phase 3 (integration_tui.rs, ~80s)
can follow once this lands without breakage. The bead asks
for ≥60s saved and no new flakes; current pace puts the
target within reach across 2 more increments.
