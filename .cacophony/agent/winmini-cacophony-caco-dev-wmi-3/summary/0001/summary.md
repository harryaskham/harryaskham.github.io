# Session summary — bd-18886a bd_dispatch_* streaming-endpoint mocks

## Goal

Fix the three `bd_dispatch_*` regression tests that were left stale
after bd-991d07 added a streaming-dispatch SSE endpoint ahead of the
legacy claim-and-spawn flow. These tests panicked their mock servers
on the first accept(), then cascaded into a CliError on the main
thread, and (pre-bd-b7a3c5) also leaked a cleared managed-env
environment into subsequent tests.

## Bead(s)

- `bd-18886a` — [broken-on-main] bd_dispatch_* tests stale after
  bd-991d07 streaming-dispatch refactor
- Sibling (landed in prior session): `bd-b7a3c5` — RAII env guard so
  panics no longer leak cleared env.

## Before state

- `cargo test -p caco-cli --lib -- --test-threads=1 tests::bd_dispatch`
  → 0 passed, 3 failed.
- Each test panicked with `expected request prefix 'POST .../beads/claim ',
  got: POST .../beads/dispatch HTTP/1.1` then the CLI errored out on
  the downed mock thread.

## After state

- `cargo test -p caco-cli --lib -- --test-threads=1 tests::bd_dispatch`
  → 3 passed, 0 failed (7.7 s).
- The three mock servers now synthesize a 404 for the streaming
  dispatch request, which the CLI treats as
  `StreamDispatchOutcome::UnsupportedByDaemon` and falls through to
  the legacy flow the remaining mock responses already describe.

## Diff summary

- Commit: `adfd9e300 bd-18886a: mock 404 for streaming-dispatch
  endpoint in bd_dispatch tests`
- Files touched: `crates/caco-cli/src/lib.rs` (+33 / -0 in the three
  test bodies).
- No production code changed; test-only fixture additions.

## Operator-takeaway

This is the mirror of the bd-b7a3c5 fix: the panic-leak fix makes
the failure *local*; this fix makes the failure *go away*. With both
landed, the three-test stale set is green and the downstream 10
"env-isolation" tests are green because no panic can leak env.
Pattern to remember: when adding a new endpoint ahead of an existing
flow, grep for every test that mocks the existing flow and thread a
synthetic "unsupported" response through each one, or the daemon
contract will silently drift the tests red.
