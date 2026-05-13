# Session summary — messages/send backpressure recurrence

## Goal

Resolve the P1 `messages/send` endpoint recurrence where direct coordination attempts timed out or returned endpoint transport errors while cheap daemon reads and board reads remained healthy. The session focused on preserving the newer mainline fix that already made the endpoint return structured `msg_send_backpressured` diagnostics, then adding bead-specific regression coverage and SPEC traceability for `bd-78224c`.

## Bead(s)

- `bd-78224c` — messages/send endpoint transport errors recur while reads are healthy

## Before state

- Failing tests: none from this checkout; live recurrence evidence on the bead showed many `caco msg send` attempts failing before response despite healthy `/api/v1/node`, service status, and bead status reads.
- Relevant metrics: current `origin/main` already contained a richer message-send backpressure implementation from related beads: a messages/send-specific 10s daemon timeout, `msg_send_backpressured` envelopes, retry metadata, and `Retry-After` headers.
- Context: the initial local patch overlapped with those newer mainline changes and produced a rebase conflict in `SPEC.md` and `crates/caco-daemon/src/lib.rs`.

## After state

- Failing tests: none observed in targeted validation.
- Relevant metrics: added a `bd-78224c` regression test that asserts request-timeout backpressure envelopes are retryable, carry reason `request_timeout`, and include `timeout_ms`; SPEC 15.3 now references `bd-78224c` in the bounded direct-message acceptance rule.
- Context: rebase resolution kept the richer mainline implementation instead of reintroducing duplicate helper functions, then layered only the bead-specific traceability and regression assertion.

## Diff summary

- Code/content commits: `79c6b2339` (`bd-78224c: bound messages send route timeout`)
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `SPEC.md`; `crates/caco-daemon/src/lib.rs`; `.cacophony/agent/winmini-cacophony-caco-dev-wmi-1/summary/pending/summary.md`
- Tests: +1 / -0 / flipped 0
- Validation: queued `caco test run --project cacophony --wait --command "cargo test -p caco-daemon --lib bd_78224c -- --nocapture" --cwd "$PWD"` passed as `tj-32cfa01d`; source assertions for the regression/SPEC text; `git diff --check origin/main..HEAD`.
- Behavioural delta: the existing mainline endpoint fix remains the product behaviour; `bd-78224c` now has explicit regression coverage and specification traceability so this recurrence stays tied to the structured retryable `msg_send_backpressured` path.

## Operator-takeaway

The important outcome is that the newer mainline backpressure fix was preserved rather than overwritten, and `bd-78224c` now has a targeted test receipt proving the recurrence class maps to retryable structured diagnostics instead of unclassified message-send transport hangs.
