# Session summary — bd-b7a3c5 env-isolation RAII guard

## Goal

Address the core of bd-b7a3c5 ("caco-cli test suite has ~83/848
env-isolation failures: tests pass standalone, fail under shared
mutable env"). The premise in the bead description was partly wrong:
only ~10 of the failures are env-isolation bleed; the rest are
genuine stale-test / logic regressions that surface separately. This
session fixes the env-isolation pattern for the 10 that match, and
files broken-on-main beads for the rest.

## Bead(s)

- `bd-b7a3c5` — caco-cli env-isolation failures (this session)
- Filed (queued): `[broken-on-main] bd_dispatch_* tests stale after
  bd-991d07 streaming-dispatch refactor` (3 tests)
- Filed (queued): `[broken-on-main] choices_mcp_tool_entries_have_three_tools
  expects 5, got 6`
- Filed (queued): `[broken-on-main] changelog_branch_root_renders_help_json:
  bd-02c5f7 no_subcommand envelope vs bd-2wn help-render`

## Before state

- `cargo test -p caco-cli --lib --test-threads=1` produced 15 failures
  on the `tests::` path.
- Standalone replay showed only 5 of those actually failed in isolation
  (3 bd_dispatch_*, choices_mcp_tool_entries_have_three_tools,
  changelog_branch_root_renders_help_json).
- The other 10 (bd_update_*, bd_snapshot_*_with_empty_stamp_rejected_bd140660)
  passed standalone but failed under the suite with
  `CliError { message: "unable to resolve local node" }`.

## After state

- `cargo test -p caco-cli --lib "tests::bd_" -- --test-threads=1`:
  90 passed, 3 failed (only the stale bd_dispatch_* set).
- The 10 env-leak failures are gone. Root cause diagnosis in commit
  message.
- 3 broken-on-main beads queued for the remaining real regressions.

## Diff summary

- Commit: `360c1e16d bd-b7a3c5: RAII guard for managed-agent env vars`
- Files touched: `crates/caco-cli/src/lib.rs` (+43 / -9 in the test
  helpers module)
- Behavioural delta: `clear_managed_env()` now returns a
  `ManagedEnvGuard` with a `Drop` impl that restores saved env vars
  even on panic. `restore_managed_env()` kept as a no-op for
  call-site compatibility (60+ existing call-sites unchanged).
- Tests: no new tests (the existing ~30 call sites validate the
  behaviour). +1 docstring example on `clear_managed_env`.

## Operator-takeaway

The bd-b7a3c5 description overstated the failure count (83 ≠ 10) and
conflated two orthogonal problems: test-local env leakage on panic
(this fix, RAII guard) and stale/broken tests that merely leaked env
state as a *consequence* of their own panics. Splitting them out
turned a 15-failure "massive env-isolation audit" into a
one-line-Drop-impl fix plus three tidy broken-on-main filings. Next
reflection: the pattern of "panic after clear_managed_env" is
silently dangerous — a lint or test-harness assertion that env vars
are pristine at test-start would catch this class of bug at the
leaky test itself, not cascaded downstream tests.
