# Session summary — prune delete cache-race stabilization

## Goal

Stabilize the `caco prune run --delete` test path against checkout-size cache races without broadening scope beyond prune/destructive-delete behavior.

## Bead(s)

- `bd-742237` — Stabilize caco prune delete tests against async checkout-size cache races

## Before state

- `bd-dcafee` validation had exposed an order-dependent prune test failure: `prune_run_delete_can_target_discarded_agent_dirs` could fail when run with the full `prune` filter because `scan_agents_dir_all` spawned asynchronous checkout-size cache writers in agent directories that delete mode was trying to remove.
- The temporary fix used process-wide `CACO_DISABLE_ASYNC_CHECKOUT_SIZE_CACHE` suppression around prune delete inventory scans, but process-wide env remains fragile in a parallel test binary and does not encode the destructive-scan contract in the API.

## After state

- Added `scan_agents_dir_all_without_size_cache`, a daemon agent inventory scan that includes discarded records but never spawns checkout-size cache writer threads.
- `caco prune run --delete` now uses that no-background scan directly instead of setting process-wide env.
- Non-delete prune/list paths keep the existing scan behavior and still populate the best-effort size cache.
- Added regression coverage proving the no-background scan does not create `checkout_size_bytes` sidecar files in removable agent directories.

## Diff summary

- Commits: agent-branch code commit for `bd-742237` plus this summary commit; final mainline squash SHA is assigned during reintegration.
- Files touched: `crates/caco-daemon/src/agent/mod.rs`, `crates/caco-daemon/src/agent/tests.rs`, `crates/caco-cli/src/lib.rs`, `.cacophony/agent/ms-dev-cacophony-caco-dev-msd-4/summary/pending/summary.md`
- Tests: +1 daemon regression test for the no-background scan; existing prune tests exercised through the full `prune` filter.
- Validation: `cargo fmt --all -- --check` passed; queued job `tj-02a57999` passed `RUST_MIN_STACK=33554432 cargo test -p caco-cli prune --lib && cargo test -p caco-daemon bd_742237 --lib`; post-rebase retryable infrastructure job `tj-d4b6574c` was recovered after daemon restart with no stale workers found; retry `tj-72be238c` passed the same command.
- Behavioural delta: destructive prune no longer relies on process-global env to avoid async cache writers racing with `remove_dir_all`.

## Operator-takeaway

The stable contract is now explicit: destructive prune inventory is a synchronous metadata-only scan. Size-cache refresh remains available for normal inventory surfaces, but delete mode no longer starts background writers inside directories it is about to remove.
