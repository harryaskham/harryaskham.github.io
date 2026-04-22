# Summary 0012 — bd-c70e2e: /api/v1/ui/snapshot ETag + 304 fast-path

## Bead
bd-c70e2e (P3, feature) — ETag + If-None-Match → 304 fast-path for
the snapshot. Self-filed follow-up to bd-ecf1a0.

## Approach

The snapshot necessarily contains live timestamps (telemetry,
freshness markers, recent_events) and per-request fields
(request_id) that defeat any naive whole-body hash. Hash a
**structural** digest of just the high-bytes fields whose changes
drive operator-visible state: bead IDs+statuses, agent IDs+states,
persistent_agents IDs+states, project list+config_hash, mode_state,
notification counts.

Excluded from the hash (would always change): telemetry, freshness,
recent_events, queued spawn timestamps, request_id.

Result: stable ETag across two requests to the same logical state.
Operators with a tab idle (no bead/agent activity) skip ~300 KB
re-transmission per 60s poll. Server still pays the build cost (deferred
to bd-dac14b memoisation).

## Change

`crates/caco-daemon/src/ui_stream.rs`:

- New pure `compute_snapshot_structural_etag(&UiSnapshot) -> String`
  helper. Sha256 over a sorted, canonical JSON digest of the
  structural fields; truncated to 16 bytes (32 hex chars) for compact
  ETags. Format: `"<hex>"` per RFC 7232.

- `handle_ui_snapshot_inner`:
  - Now returns `axum::response::Response` (was `impl IntoResponse`)
    so we can attach ETag + Cache-Control headers and switch status
    to 304.
  - Captures `If-None-Match` from request headers up front.
  - Builds the snapshot as before (no early-exit on ETag — full
    build is required to compute the structural hash; the saving is
    on response transmission only at this stage).
  - Computes structural ETag.
  - If `If-None-Match` matches: returns 304 Not Modified with the
    ETag echoed in the response so clients can keep using it. Empty
    body.
  - Otherwise: returns 200 with `Content-Type: application/json`,
    `ETag: "<hash>"`, `Cache-Control: private, no-cache,
    must-revalidate` (private prevents shared caches; no-cache +
    must-revalidate force revalidation on every request).

`crates/caco-web/static/app.js`:
- `loadSnapshot` now sends `If-None-Match: <last_etag>` when
  available.
- On 304: refreshes `lastSnapshotTime` + connection-status pulse,
  returns early without re-applying snapshot (cached state is
  still authoritative).
- On 200: captures the new ETag from response headers for the
  next request.

## Tests

`crates/caco-daemon/src/lib.rs::tests::`:

- `ui_snapshot_returns_etag_header` — happy path: 200 with quoted
  ETag and Cache-Control: no-cache.
- `ui_snapshot_returns_304_when_if_none_match_matches` — first
  request gets ETag, second with matching If-None-Match returns
  304 + empty body + echoed ETag.
- `ui_snapshot_returns_full_body_on_etag_mismatch` — wrong ETag
  yields 200 with full body.
- `ui_snapshot_etag_is_stable_across_requests` — explicit guard
  that two requests with different `x-caco-request-id` values still
  produce identical ETags. This is the regression-prevention test
  for the structural-only-hash approach.

## Verification

- `cargo check -p caco-daemon` — clean.
- `cargo test -p caco-daemon --lib ui_snapshot` — 5/5 green
  (4 new + 1 pre-existing).
- `cargo test-small` — 4211/4211 green
  (204+109+720+291+18+2815+54 across small-suite crates).

## Operational impact

- Operators with idle dashboard tabs skip ~300 KB raw / ~75 KB
  gzipped retransmission per 60s poll.
- Server still pays the build cost (~few hundred ms after
  bd-ecf1a0 trim). bd-dac14b will reuse the structural ETag as
  the memoisation cache key so subsequent fetches in the same
  build window are sub-50ms even on miss.
- Cache-Control: private — operator-scoped data must never be
  cached by intermediaries.
- ETag is a strong validator (no weak prefix); structural hash
  guarantees `If-None-Match` matches only when the operator-visible
  state is unchanged.

## Deferred

- bd-dac14b (server-side memoisation): the structural ETag here
  becomes the cache key. Build the full body once per N seconds,
  serve identical bytes (and identical ETag) to all callers in
  the window.
- bd-f8ae0d (tab-hidden client backoff): Pages that respect
  document.visibilityState will benefit doubly — fewer requests
  AND those requests are cheap 304s when fired.

## Next

Reintegrate direct, close bd-c70e2e, idle.
