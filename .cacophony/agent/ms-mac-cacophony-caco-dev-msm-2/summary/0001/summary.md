# Session summary — snapshot bootstrap 24h cutoff (bd-a7168d)

## Goal

Stop newly-opened TUI / web / app sessions from showing huge "pop-in" of
multi-day-old activity (chat, broadcasts, feed events, speech) on first
paint. The live SSE delivery path was always fine; the problem was the
snapshot bootstrap pulling unbounded history from the persisted store.
Default the bootstrap window to last 24h and let callers widen it via an
explicit `since=` query param.

## Bead(s)

- `bd-a7168d` — Opening SSE streams shows huge pop-in of old items; views should default to last 24h
- (filed during preflight: `bd-03a276` — broken-on-main tmux-socket flake in `persistent_recreate_relaunches_project_controller_replacement`, unrelated to this change)

## Before state

- Failing tests on touched code: none (`cargo test -p caco-daemon --lib store::tests::query_` passes pre-change too).
- `handle_ui_snapshot_inner` pulled `recent_events` (50), `chat_history` (200), `speech_history` (50) without any age bound, so any persisted history appeared as fresh state on first paint.
- `GET /api/v1/feed` returned the most recent 100 rows regardless of age, with no `since` / `limit` parameters.
- bd-c55c0c had already added a 24h cap on cross-peer chat-history shipment via `bound_chat_history_by_age`, but only on the replication path — not on the snapshot or feed endpoints.

## After state

- `cargo test -p caco-daemon --lib store::tests::query_` — 11 passed, 0 failed (including 2 new tests).
- `cargo test-small` across the workspace — 195 / 109 / 716 / 277 / 18 / 2780 / 45 passed, 0 failed.
- Pre-existing tmux-socket flake `persistent_recreate_relaunches_project_controller_replacement` filed as bd-03a276; not caused by this change.
- Snapshot bootstrap and `GET /api/v1/feed` now default to a 24h window. Callers that need the full history can pass `?since=all` or an explicit RFC 3339 cutoff.

## Diff summary

- Commits: `8227cff0`
- Files touched:
  - `crates/caco-daemon/src/store.rs` — added `query_recent_events_since(Option<&str>, limit)` and `query_events_by_types_since(types, Option<&str>, limit)`. The existing `query_recent_events` and `query_events_by_types` are now thin wrappers that pass `None`. Added two unit tests.
  - `crates/caco-daemon/src/ui_stream.rs` — `handle_ui_snapshot_inner` now computes `now - 24h` once and feeds it to all three of `recent_events`, `chat_history`, `speech_history`.
  - `crates/caco-daemon/src/lib.rs` — `handle_feed` now takes a `FeedQuery { since, limit }` extractor; defaults to last 24h / 100 rows / max 1000; `?since=all` disables the cap.
- Tests: +2 unit (`query_recent_events_since_drops_stale_events`, `query_events_by_types_since_drops_stale_events`); 0 removed; 0 flipped.
- Behavioural delta: first-paint history is bounded; live event delivery unchanged.

## Operator-takeaway

The fix is server-side and intentionally narrow: it only caps the bootstrap snapshot and the `/api/v1/feed` reply, not the live SSE stream. Any TUI / web / app surface that wants a wider window can already pass `?since=<rfc3339>` or `?since=all` to `/api/v1/feed`. UI date-range pickers across TUI / web / app remain follow-up work — the bead title called for them, but they're separate per-surface pieces best landed independently. Anything that today grabs every chat row from the snapshot will now see 24h worth of rows; that's the desired user-facing behaviour.
