# Session summary — socket-death managed-agent transition + broken-on-main fix

## Goal

Close the gap where a non-persistent / pruned-checkout managed agent with a confirmed dead tmux socket stayed recorded running-but-dead indefinitely, and unblock the merge-queue gate.

## Bead(s)

- `bd-be60bb` — [bd-28d361 slice 2/2] Transition socket-dead non-persistent/pruned-checkout agents out of running
- `bd-ae7543` — [broken-on-main] orphaned FILE_CACHE_GET_TIMEOUT const (dead_code) blocking the clippy gate

## Before state

- The heartbeat sweep classified a confirmed dead per-agent tmux socket as `TmuxSocketMissing`, but only the persistent-sentinel recovery route (`collect_runtime_health_persistent_failures` → `schedule_runtime_health_recovery`) acted on it. A non-persistent / orphaned managed agent with a dead socket was never transitioned out of `running` (bd-28d361 hypothesis a; I authored the original root-cause analysis on bd-28d361, slice 1 landed crediting "po4-1 analysis #3").
- `cargo clippy -p caco-daemon --lib -- -D warnings` failed on `FILE_CACHE_GET_TIMEOUT` (never used) — leftover from the file_cache.rs E0428 dedup that removed winmini's bd-95eddd shell-out handler. The merge-queue gate runs clippy -D warnings, so this blocked all caco-daemon reintegrations fleet-wide.

## After state

- `runtime_sweep::should_fail_socket_dead_managed_agent(cause, is_recovery_eligible_persistent)`: a pure, unit-tested decision helper. Returns true only for the confident `TmuxSocketMissing` cause (inconclusive/backpressured probes classify as `ProbeInconclusive` first, bd-164ebd, so it cannot flap a live-but-slow agent) AND only when the agent is NOT a recovery-eligible declared persistent (those stay on the sentinel recovery route). 4 unit tests.
- `agent/lifecycle.rs` heartbeat sweep: `HeartbeatCandidate` now carries `is_recovery_eligible_persistent` (`persistent_id.is_some() || agent_kind == Persistent`). On a confirmed dead socket for a non-persistent agent, the sweep transitions it to `Failed` (mirroring the proven pane-error-detection transition pattern), persists, and pushes to `failed_agents` so its beads are unclaimed. Declared persistents are unchanged (sentinel route owns them).
- Removed the orphaned `FILE_CACHE_GET_TIMEOUT` const so the clippy gate is green again.

## Diff summary

- Commits: `e8b38ae31` (bd-ae7543), `0429e0d04` (bd-be60bb); final landed squash SHA from the reintegration receipt.
- Files: `crates/caco-daemon/src/agent/runtime_sweep.rs` (helper + 4 tests), `crates/caco-daemon/src/agent/lifecycle.rs` (candidate field + transition), `crates/caco-daemon/src/file_cache.rs` (remove dead const).
- Validation: `cargo test -p caco-daemon --lib bd_be60bb` (4 pass), `cargo clippy -p caco-daemon --lib -- -D warnings` clean.

## Scope boundaries

- Slice 1 (false-healthy masking: don't refresh last_tool_activity on a dead socket) and hypothesis b (a declared persistent with a pruned/empty checkout looping Pending in the recovery route) are separate; this slice closes hypothesis a (the non-persistent gap) with a unit-testable transition.
- Live-repro confirmation against ms-mac `86r5xxkv7fxgjndx` follows the bd-074eac panic-fix deploy (the agent is under the active do-not-discard/stop guard until then). This is a daemon health-reconciler change (bd-7189fc family); the transition is intentionally gated on the confident `TmuxSocketMissing` signal so it cannot flap live-but-backpressured agents.

## Operator-takeaway

A non-persistent managed agent whose tmux socket dies is now transitioned to Failed within a bounded sweep instead of lingering recorded running-but-dead, and its beads are unclaimed for re-pickup. Declared persistents keep their existing recovery behavior. The clippy gate is unblocked.
