# Session summary — bd-66b2ff agent registry lock cleanup

## Goal

Reduce the ms-mac daemon backpressure class where authenticated daemon reads such as `/api/v1/node` can hang behind persistent-agent reconciliation. The focused code change removes one stale persistent-agent cleanup path that held the global agent registry lock while running tmux probe/kill helpers.

## Bead(s)

- `bd-66b2ff` — ms-mac daemon backpressure and STT watchdog restart loop after revive

## Before state

- Failing symptom: incident evidence showed authenticated requests hanging post-auth/pre-handler while unauthenticated requests returned quickly; related investigation pointed at handlers waiting for `state.agents.list_all().await` because `agents.inner` could be held by lifecycle work.
- Code context: `AgentManager::cleanup_stale_persistent_agent` held `agents.inner` while calling `verify_tmux_alive_on` and `kill_tmux_session_on`, both subprocess-backed tmux helpers that can stall on a wedged per-agent tmux socket.
- Validation context: no full suite was run locally because this shared host requires queued heavyweight Rust validation.

## After state

- `cleanup_stale_persistent_agent` now snapshots the relevant `AgentInfo` and `agents_dir`, releases `agents.inner`, performs tmux liveness/cleanup outside the registry lock, then reacquires the lock only for the final authoritative index/map removal.
- A race guard checks that the persistent-id index still points at the same stale agent after tmux cleanup, so a concurrent launcher/reconcile pass cannot have its new row removed by the old cleanup attempt.
- Validation: queued focused test passed with job `tj-7b75667d` using `RUST_MIN_STACK=33554432 CARGO_BUILD_JOBS=2 cargo test -p caco-daemon --lib cleanup_stale_persistent_agent -- --test-threads=1` (4 passed, 4032 filtered out). An earlier queued attempt `tj-a702c514` timed out because I mistakenly queued the `just caco-daemon-helper-test ...` wrapper itself.

## Diff summary

- Code/content commits: `24f4fc23a` (`bd-66b2ff: avoid agent registry lock during stale cleanup tmux probes`)
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `crates/caco-daemon/src/agent/lifecycle.rs`
- Tests: +0 / -0 / flipped 0; reused existing focused cleanup tests
- Behavioural delta: persistent-agent stale cleanup still discards stale terminal/dead-tmux rows, but no longer blocks global agent inventory reads behind tmux subprocess work.

## Operator-takeaway

This is a targeted lock-scope fix for the authenticated API wedge class behind bd-66b2ff, not a full ms-mac incident recovery. It should reduce `/api/v1/node` backpressure during persistent reconciliation; live ms-mac status/STT stability still need observation after the fixed binary lands and is deployed.
