# Session summary — bd-fc7dca UI snapshot blocking-section offload

## Goal

Implement the build-side daemon fix for reopened `bd-fc7dca`: make `/api/v1/ui/snapshot` stop returning raw empty replies on slow/wedged nodes by moving synchronous SQLite/filesystem sections off Tokio worker threads so the existing outer snapshot timeout can produce a semantic response. cs-2 remains the live validation and close owner because cs-2/ms-mac reproduce the fault; ms-dev does not.

## Bead(s)

- `bd-fc7dca` — Fix ui snapshot hang blocking iOS simulator autoconnect

## Before state

- ms-dev control probe was healthy: `/api/v1/ui/snapshot` returned HTTP 200 with a ~2.46MB body in ~4.5s.
- ms-mac and cs-2 reproduced the live blocker: `/api/v1/node` stayed healthy, but `/api/v1/ui/snapshot` returned empty reply / 0 bytes.
- Root cause from cs-2: `handle_ui_snapshot_inner` had synchronous SQLite/filesystem sections with no await point, so `tokio::time::timeout(UI_SNAPSHOT_TOTAL_DEADLINE, ...)` could not fire while a worker was pinned in blocking code.

## After state

- The feed/notification/chat/speech store-backed section now runs in `tokio::task::spawn_blocking` using `blocking_lock()` inside the closure.
- The beads-host per-project `BeadsStore::open_without_git` + `list_beads` loop now runs in `spawn_blocking`.
- The non-host local warm bead mirror scan is split into owned inputs and a blocking helper, then awaited through `spawn_blocking`.
- The operator inbox `query_items` / `query_archived_ids` section now runs in `spawn_blocking`.
- The fleet snapshot read-model cache persistence now runs in `spawn_blocking`.
- A source-level regression test pins the offloaded sections so the timeout path remains cancellable once the unrelated daemon test-fixture compile drift is fixed.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched:
  - `crates/caco-daemon/src/ui_stream.rs`
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/summary.md`
- Tests: added one source-level regression test `ui_snapshot_blocking_sections_use_spawn_blocking_bd_fc7dca`.
- Validation:
  - `./scripts/rustfmt-changed.sh --check crates/caco-daemon/src/ui_stream.rs` — pass
  - `cargo check -p caco-daemon --lib` — pass
  - `cargo test -p caco-daemon --lib ui_snapshot_blocking_sections_use_spawn_blocking_bd_fc7dca -- --test-threads=1` — blocked by pre-existing current-main daemon test fixture compile drift (`Config.client_nodes` and `ProjectAgentsConfig.groups` missing in explicit test initializers), not by this patch.
- Behavioural delta: slow synchronous snapshot sub-sections now yield at `spawn_blocking` JoinHandle awaits, so the existing 5s outer timeout can return the structured `ui_snapshot_timeout` response instead of clients observing a raw empty reply.

## Operator-takeaway

This patch is the build-side half of the reopened snapshot blocker: it makes the daemon snapshot path cancellable under slow store/filesystem work. The real proof still has to happen on cs-2/ms-mac after release/update, because ms-dev’s store is healthy and returns 200 rather than reproducing the empty-reply failure.
