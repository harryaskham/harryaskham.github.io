# Session summary — queued wait position visibility

## Goal

Improve `caco test run --wait` visibility so an agent waiting behind queued validation work can see queue placement before an outer tool timeout. I implemented the shared progress helper for both test and build waits because the two queue surfaces have the same operator-facing blind spot.

## Bead(s)

- `bd-1cd621` — Show queued-test position while caco test run --wait is waiting

## Before state

- Failing tests: none known for this change at start.
- Relevant metrics: while validating related queue work, agents had to fall back to `caco test list/show` after `caco test run --wait` timed out without queue position or ahead-job hints.
- Context: the existing wait loop polled the specific job until terminal state or timeout, but did not consult the list surface for queued/running placement while waiting.

## After state

- Failing tests: none in focused validation.
- Relevant metrics: `tj-5c4e5c55` passed for `RUST_MIN_STACK=33554432 cargo test -p caco-cli queue_wait_progress_reports_position_and_running_count_bd_1cd621 -- --nocapture`; `cargo fmt --all` and `git diff --check` passed.
- Context: human-readable `caco test run --wait` and `caco build run --wait` now emit deduplicated progress lines for queued/running jobs when the queue list is available. JSON timeout payloads include `data.wait_progress` with state, queued position, queued-ahead count, queued total, running count, ahead job IDs, and a human summary.

## Diff summary

- Commits: `3d5cb8e3f6` (`bd-1cd621: show queued wait position`)
- Files touched: `crates/caco-cli/src/lib.rs`, `SPEC.md`, `README.md`
- Tests: +1 caco-cli unit test for queue wait progress derivation and JSON summary shape.
- Behavioural delta: wait loops still return terminal job results exactly as before, but intermediate human-readable waits and JSON timeouts now carry queue placement metadata when available.

## Operator-takeaway

Queued validation waits should now be less opaque: when work is stuck behind other jobs, agents can see their queue position and running-count context without manually issuing a separate list/show command.
