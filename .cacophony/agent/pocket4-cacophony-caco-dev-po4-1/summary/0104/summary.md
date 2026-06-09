# Session summary — daemon project file-cache read surface (list + content)

## Goal

Give first-party clients (caco-web proxy, loopback, mobile) a canonical daemon HTTP read surface for the project file-cache, which had no daemon list/content routes.

## Bead(s)

- `bd-6eaabe` — Daemon file-cache content endpoint (GET /api/v1/file-cache/<id>/content) + list (this session's primary work)
- `bd-f4474c` — Daemon file-cache UPLOAD endpoint: landed independently by msm-1 (commit 6c28184cc) while I investigated; I reopened/claimed it during a brief race, then re-closed it as already-landed. Not re-implemented here.
- `bd-c798ec` — broken-on-main caco-daemon clippy fix (unblocks the merge-queue gate)

## Before state

- The daemon exposed NO `/api/v1/file-cache` routes at all (no list, content, or upload). The iOS kit `DaemonClient.fileCache()` GETs `/api/v1/file-cache` against the daemon, which would 404.
- caco-web served blobs only via its own `/api/v1/web/files/<project>/<id>/content` — mobile/mTLS clients cannot use it.
- During this work, msm-1 landed the daemon UPLOAD endpoint (`POST /api/v1/file-cache`, delegating to `caco file add`) at 6c28184cc. The READ surface (list + content) was still missing.
- `cargo clippy -p caco-daemon --lib -- -D warnings` was broken-on-main (doc-list-indent in agent/mod.rs + redundant_closure in agent/health.rs), which would block the merge-queue gate.

## After state

- Extended `crates/caco-daemon/src/file_cache.rs` (msm-1's upload module) with the read surface, registered on `local_router` (bearer; mirrors `/api/v1/images` + the upload POST):
  - `GET /api/v1/file-cache` — list records with `project`/`query`/`limit` filters; snake_case envelope `{scope, project, count, files[]}` matching the iOS `FileCacheResponse` decoder; tombstoned records filtered, `available` recomputed from blob existence; newest-first.
  - `GET /api/v1/file-cache/{id}/content` — streams blob bytes with `Content-Type` (record mime), `Content-Disposition: attachment; filename=...`, `Cache-Control: no-store`; optional `?project=` scoping, else searches all project dirs.
- Reads the canonical on-disk store `$CACOPHONY_DIR/files/<sanitized-project>/index.jsonl` + `blobs/<sha256>` (same contract as `caco file` CLI / caco-web), so it serves records written by the CLI and by msm-1's upload endpoint.
- Cluster mTLS (client_nodes) exposure of these routes is intentionally deferred to the bd-754c27 operator-gated per-cert authz decision (documented in the module + route comments).
- Fixed the broken-on-main caco-daemon clippy errors (bd-c798ec) so the gate passes.

## Diff summary

- Read endpoints commit: `11dc8c4d3`; broken-on-main fix commit: see branch; final landed squash SHA comes from the reintegration receipt.
- Files: `crates/caco-daemon/src/file_cache.rs` (read handlers/helpers/tests added), `crates/caco-daemon/src/lib.rs` (GET list method + content route), `crates/caco-daemon/src/agent/mod.rs` + `agent/health.rs` (broken-on-main clippy fix).
- Validation: `cargo test -p caco-daemon --lib file_cache::` (12 tests pass, 9 upload + 3 read), `cargo clippy -p caco-daemon --lib -- -D warnings` clean.

## Coordination

- Found the daemon file-cache surface missing + bd-f4474c closed-but-unlanded; broadcast the gap and looped in caco-ios-2 (Share-extension owner) + caco-ios-1. msm-1 then landed the upload independently; I rebased onto it and kept only the read surface. The list/content/upload contract (request/response shapes) is what caco-ios-2's appex needs.

## Operator-takeaway

The daemon now owns the full project file-cache API (msm-1's upload + this read list/content) over the canonical on-disk store, unblocking mobile blob download (bd-d812ad) and the Share-extension. Cluster-mTLS exposure for client_nodes still awaits the bd-754c27 authz decision.
