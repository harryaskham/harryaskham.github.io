# Session summary — streaming bead dispatch now authenticates its internal spawn hop

## Goal

Fix a throughput-blocking helsinki bug where `caco bd dispatch` could claim a
bead and then fail to spawn the worker with `spawn http 401 Unauthorized:
Invalid or missing authorization`. The operator-facing goal was to make local
controller dispatch work again on helsinki and to ensure failed spawns do not
leave beads stranded `in_progress`.

## Bead(s)

- `bd-cfa3c7` — caco bd dispatch on helsinki returns spawn http 401 Unauthorized (cannot start agents)

## Before state

- `crates/caco-daemon/src/dispatch_stream.rs::run_dispatch_pipeline(...)`
  claimed the bead and then loopback-posted to
  `POST /api/v1/projects/<project>/agents`.
- That internal loopback request sent `x-caco-caller` provenance but **no
  bearer auth**, so the local protected spawn endpoint rejected it with `401
  Unauthorized: Invalid or missing authorization`.
- When that definitive spawn failure happened, the streaming dispatch path left
  the bead claimed, so operators/controllers had to manually unclaim it before
  the backlog could move again.

## After state

- The internal spawn hop now authenticates with `state.bearer_token`, so the
  local protected spawn endpoint sees an authorized internal request.
- A new rollback helper now best-effort unclaims the bead on definitive spawn
  failures that occur after the dispatch pipeline has already claimed it.
- The streaming path now also handles `ok: false` spawn envelopes explicitly
  instead of only checking HTTP status and `data.id`.
- Regression coverage now exercises the real dispatch pipeline against a live
  local test router using an intentionally invalid profile so the failure occurs
  after auth; the test asserts the error is no longer the generic missing-auth
  failure and that the bead is restored to `open`.

## Diff summary

- Files touched:
  - `crates/caco-daemon/src/dispatch_stream.rs`
  - `crates/caco-daemon/src/lib.rs`
- Behavioural delta:
  - streaming `bd dispatch` no longer self-401s when it loopback-calls the
    local spawn endpoint
  - definitive post-claim spawn failures no longer strand beads in
    `in_progress`
- Validation:
  - `cargo test -p caco-daemon bead_dispatch_stream_internal_spawn_auth_and_rollback_bd_cfa3c7 -- --nocapture`
  - `cargo build -p caco-daemon`
  - `cargo fmt --all`
  - `cargo test -p caco-daemon bead_dispatch_stream_internal_spawn_auth_and_rollback_bd_cfa3c7 -- --nocapture` (post-format recheck)

## Operator-takeaway

This was a real daemon-side bug in the streaming dispatch implementation, not a
missing operator privilege. The dispatch pipeline was calling its own protected
spawn endpoint without auth, then leaking the bead claim on failure. That path
is now authenticated and rollbacks are automatic, which should unblock local
controller-driven burndown on helsinki.
