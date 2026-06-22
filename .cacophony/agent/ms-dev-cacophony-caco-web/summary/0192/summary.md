# Session summary — bd-fc7dca UI snapshot bounded timeout

## Goal

Fix the `/api/v1/ui/snapshot` hang that blocked iOS simulator autoconnect validation. The daemon could answer `/api/v1/node` quickly while `/api/v1/ui/snapshot` held the HTTP socket open with zero response bytes for longer than mobile clients tolerate.

## Bead(s)

- `bd-fc7dca` — Fix ui snapshot hang blocking iOS simulator autoconnect.

## Before state

- Host repro with node token: `/api/v1/node` returned HTTP 200 in ~0.22s.
- Host repro with same token: `/api/v1/ui/snapshot` timed out after 10s with 0 bytes.
- Router/caco-ctrl evidence matched iOS simulator behaviour: node endpoint reachable from host and simulator; snapshot endpoint silent for >20s.
- Code inspection showed the handler bounded mesh bead fan-out and agent reconcile, but still had unbounded shared store-lock reads in store-backed snapshot sections and no total handler deadline.

## After state

- `handle_ui_snapshot` now wraps the entire inner handler in `UI_SNAPSHOT_TOTAL_DEADLINE` (5s). If any later subview stalls, the daemon returns a structured HTTP 504 JSON envelope with `code: ui_snapshot_timeout`, `Retry-After: 2`, `backend_unavailable: true`, and `x-cacophony-snapshot-cache: timeout` instead of leaving clients with 0 bytes.
- Initial event/notification/chat/speech store-backed sections now use `UI_SNAPSHOT_STORE_LOCK_DEADLINE` (200ms). If the shared store lock is unavailable, the snapshot degrades those sections to empty and continues building the rest of the response.
- Operator inbox and archived inbox IDs now use the same bounded store-lock pattern so the later struct-construction section cannot reintroduce the hang.
- Existing cache/stale-while-rebuild/startup-backpressure paths remain unchanged.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted.
- Files touched:
  - `crates/caco-daemon/src/ui_stream.rs`
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/summary.md`
- Tests:
  - `caco test run --wait --command "cargo test -p caco-daemon snapshot_ -- --test-threads=1" --cwd "$PWD"` → `tj-806b648b`, passed.
- Behavioural delta: snapshot clients receive bounded degraded/timeout semantics instead of hanging silently behind contended store reads.

## Operator-takeaway

The iOS simulator path no longer has to wait for its own ~20s timeout when the daemon snapshot is wedged: the daemon either serves a degraded snapshot quickly or returns a semantic retryable 504 within 5 seconds. A deeper follow-up may still remove synchronous SQLite from the async first-paint path, but this fixes the immediate zero-byte hang.
