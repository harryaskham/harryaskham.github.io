# Session summary — bd-c19193 agent_summary tests stack overflow

## Goal

Fix the broken-on-main test failure
`agent_summary_exclude_routine_node_health_hides_mismatch_and_advisory`
in `crates/caco-cli/src/lib.rs`, which SIGABRTed with a stack overflow
under `cargo test` from a clean checkout.

## Bead(s)

- `bd-c19193` — [broken-on-main] agent_summary_exclude_routine_node_health_hides_mismatch_and_advisory stack overflow.
  - Originally announced by msd-4 (broken-on-main observation), then
    handed off to me when msd-4 chose to focus on bd-f49a71 reintegration.

## Before state

- `cargo test -p caco-cli --lib agent_summary_exclude_routine_node_health`
  reliably aborted with `thread '...' has overflowed its stack`.
- Investigation showed the sibling test
  `agent_summary_text_separates_actionable_and_advisory_node_health`
  has the identical failure mode — both call into the same
  `dispatch_agent_summary` path whose monomorphisations overflow the
  default 2 MB test-thread stack.
- Confirmed root cause by passing under
  `RUST_MIN_STACK=33554432 cargo test ...` — pure stack-budget issue.

## After state

- Both agent_summary tests wrapped in the existing
  `run_help_test_with_large_stack` helper (16 MB stack via
  `RUN_DISPATCH_STACK_SIZE` — same budget production gets in
  `pub fn run()`, established by bd-e4f3e3 / bd-a7441a).
- `cargo test -p caco-cli --lib agent_summary` — both tests pass.
- `cargo test-small` workspace-wide green; `cargo clippy -p caco-cli
  --lib --tests` clean.
- No production code changes — purely a test-runner stack fix.

## Diff summary

- Commit: `bf75e1a4` (bd-c19193: agent_summary tests overflow default
  2MB stack — wrap in run_help_test_with_large_stack).
- Files touched: `crates/caco-cli/src/lib.rs` (test wrapping only).
- Tests: +0 / -0 / flipped 2 (both moved from `#[test] fn body` to
  `#[test] fn run_help_test_with_large_stack(|| body)`).
- Behavioural delta: zero. Production dispatch path is unchanged.

## Operator-takeaway

The 16 MB `run_help_test_with_large_stack` helper has now absorbed
its 11th caller in this file. The recurring pattern is: any test that
re-enters the CLI dispatch tree (`run`, `dispatch_agent_summary`,
choices/MCP dispatch helpers) needs the larger stack because the
release path uses 16 MB. The default `#[test]` 2 MB budget is too
small for this monomorphisation tree, and it's worth treating
"new test calls into dispatch" as a coding rule that mandates the
helper. A future polish task could extract this into a custom
`#[caco_dispatch_test]` attribute macro so the helper is invoked
automatically.
