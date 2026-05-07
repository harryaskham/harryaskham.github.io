# Session summary — queued-dispatch non-open cleanup follow-up

## Goal

Determine whether the post-close queued-dispatch non-open skip signature from `bd-11fdf4` was only stale daemon-crash tail residue or still actively recurring, then keep the fix scoped to daemon queued-dispatch cleanup if active.

## Bead(s)

- `bd-41817b` — [follow-up] queued-dispatch non-open skip noise persists after bd-11fdf4
- Follow-up context: `bd-11fdf4` — queued dispatch pickup skips for in-progress beads recur in daemon-crash.log

## Before state

- `bd-11fdf4` was closed, but log-monitor still reported `daemon-crash.log` at roughly 1.09 MiB with about 2,498 tail occurrences of `bd-0c08a2: queued dispatch pickup skipped ... is not open (status: in_progress)`.
- Bounded ms-mac sampling during this session showed active recurrence rather than stale residue: the log grew from 1,057,495 bytes / 2,531 tail occurrences to 1,071,261 bytes / 2,552 tail occurrences over about 45 seconds.
- Code inspection showed queued dispatch pickup still listed target-node dispatch rows without requiring `status=open`, then attempted claims and only cleared after a claim error matching `status: in_progress`.

## After state

- Queued dispatch pickup now treats any queued dispatch whose current bead status is not `open` as stale before attempting a claim.
- Stale non-open dispatch metadata is cleared through the existing first-party routed dispatch-clear path and summarized through the non-stderr `queued_dispatch_pickup` logger rather than repeated daemon-crash stderr lines.
- The fallback claim-error matcher is status-agnostic for `is not open (status: ...)` errors, so closed/draft/permanent variants are also cleared if they reach the claim path.
- `bd-41817b` and `bd-11fdf4` descriptions were updated with the bounded active-recurrence evidence and cross-reference.

## Diff summary

- Commits: agent-branch code commit for `bd-41817b` plus this summary commit; final mainline squash SHA is assigned during reintegration.
- Files touched: `crates/caco-daemon/src/lib.rs`, `.cacophony/agent/ms-dev-cacophony-caco-dev-msd-4/summary/pending/summary.md`
- Tests: +2 targeted daemon unit tests for non-open queued-dispatch classification and status-agnostic claim-error matching.
- Validation: `cargo fmt --all -- --check` passed; queued jobs `tj-1e684b77` and post-rebase `tj-e029a9ec` passed `cargo test -p caco-daemon bd_41817b --lib`.
- Behavioural delta: stale queued dispatch intents for already in-progress/closed/draft/permanent/deleted beads are cleared before pickup instead of repeatedly logging non-crash claim failures to `daemon-crash.log`.

## Operator-takeaway

The remaining signature after `bd-11fdf4` was an active recurrence: the scanner could still see non-open queued-dispatch rows and repeatedly try to claim them. This follow-up moves the non-open check ahead of the claim, so stale queue metadata is cleaned as queue state instead of surfacing as crash-log noise.
