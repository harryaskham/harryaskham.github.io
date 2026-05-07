# Session summary — in-process beads health in caco status

## Goal

Implement `bd-c6f9fa` so high-level `caco status --json` does not report a local in-process beads host as `not_running` when the daemon itself is reachable and `caco bd status` / beads CRUD are authoritative.

## Bead(s)

- `bd-c6f9fa` — caco status reports ms-mac beads_host not_running while bd status is authoritative

## Before state

- Failing tests: none known for this bead. A peer reported a separate `caco-daemon` broken-on-main compile issue, but this bead touched `caco-cli` status logic only and the queued caco-cli checks compiled successfully.
- Relevant metrics: log-monitor evidence showed `caco status --json` on ms-mac returning `ok:false` with `daemon.reachable:true`, `beads_host.mode:"in_process"`, and `beads_host.status:"not_running"`, while `caco bd status --json` returned `ok:true`, `is_authoritative:true`, and fresh project sync.
- Context: after `bd-720b5b` landed and closed, controller assigned this bead as the next overnight burndown slice with instructions to preserve first-party status/bd semantics and avoid release/runner work.

## After state

- Failing tests: none observed in targeted validation.
- Relevant metrics: `git diff --check` passed locally; queued `CARGO_BUILD_JOBS=2 RUST_MIN_STACK=33554432 cargo test -p caco-cli --lib bd_c6f9fa -- --test-threads=2` passed (`tj-197464ad`); queued `CARGO_BUILD_JOBS=2 cargo check -p caco-cli --lib` passed (`tj-8ab9f836`).
- Context: local in-process beads health is now derived from daemon reachability even when the advisory primary-view request is missing/timed out. Standalone caco-bd-daemon mode still requires the standalone daemon to be running and a live primary view.

## Diff summary

- Commits: `19e8ef962`
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: added focused `bd_c6f9fa` unit coverage for the in-process/no-primary-view case and standalone mode guard.
- Behavioural delta: `caco status` should agree with `caco bd status` for legacy/in-process local beads hosting instead of surfacing `in_process not_running` solely because a primary-view diagnostic timed out.

## Operator-takeaway

The fix preserves the distinction between standalone and in-process beads hosting: standalone still needs the bd daemon, but in-process beads are healthy when the local caco-daemon is healthy.
