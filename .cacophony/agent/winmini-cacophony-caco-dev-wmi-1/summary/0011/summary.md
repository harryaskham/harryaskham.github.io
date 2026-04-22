# Session summary — Cache /api/v1/agents response with event-driven invalidation (bd-7a1ca5)

## Goal

bd-9d5be2 deferred per-agent `dir_size` walks off the request
path. Next latency win on the hot polling loop (TUI agent list,
caco-web, `caco status`) is to cache the rendered JSON envelope
itself in `DaemonState` and invalidate it on observable
agent-lifecycle state changes — so steady-state polls return
without re-walking the in-memory agent map, re-merging peer
snapshots, re-hashing persistent agent inventory, or re-serialising.

## Bead(s)

- `bd-7a1ca5` — [bd-9d5be2 follow-up] /api/v1/agents response
  cache invalidated on state-change events

## Before state

- Every GET `/api/v1/agents` rebuilt the whole envelope: walked
  `list_all_with_disk_refresh()`, merged remote peer snapshots,
  walked persistent agent inventory, serialised to JSON.
- Polling clients (TUI list view, caco-web, `caco agent list`)
  paid that cost on every tick.

## After state

- New `AgentsListCache` module with monotonic generation counter
  + `RwLock<Option<CachedEntry>>`.
- `DaemonState` owns `Arc<AgentsListCache>`.
- `AgentManager` gains an optional cache hook, wired once during
  `pub async fn run()`.
- Hot path in `handle_agents_list`: `cache.get()` → return; only
  on miss/stale do we run the existing rebuild.
- 4 lifecycle bump points (`set_state`, `create`, `prune`,
  `discard`) — every other state mutation goes through one of
  these.
- 9 new unit tests, all passing.

## Diff summary

- Files touched (5 files; +372 / −12):
  - `crates/caco-daemon/src/agents_list_cache.rs` (new, 233L)
  - `crates/caco-daemon/src/lib.rs`: module decl, DaemonState
    field, 14 construction sites, `run()` cache wire-up, handler
    integration.
  - `crates/caco-daemon/src/agent/mod.rs`: AgentManagerInner
    field, setter, internal bump helper.
  - `crates/caco-daemon/src/agent/lifecycle.rs`: bump in
    `set_state`, `create`, `prune`, `discard`.
  - `crates/caco-daemon/src/ui_stream.rs`: drive-by — strip 6
    duplicate `tmux_history_limit/size` lines (3 pairs) caused
    by main's bd-bce6ea backfill landing alongside my own.

### Unit tests added (9 in `agents_list_cache::tests`)

- `new_cache_starts_empty_at_generation_zero`
- `store_then_get_returns_value_when_generation_matches`
- `bump_invalidates_cached_entry`
- `store_with_old_generation_is_immediately_stale` (race-safety)
- `store_with_post_bump_generation_is_valid`
- `clear_drops_entry_without_bumping_generation`
- `multiple_bumps_are_monotonic`
- `rebuild_replaces_previous_entry`
- `concurrent_bumps_are_observed_by_subsequent_get`
  (Acquire/Release ordering smoke under 4×50 thread bumps)

## Embedded artefacts

(none — pure perf slice; no schema/CLI changes)

## Operator-takeaway

Steady-state polling on the agent inventory is now O(1) cache
read for the inner data payload. The first request after any
lifecycle change (or daemon start) pays the rebuild cost
exactly once; subsequent requests at the same generation reuse
the cached body. The `request_id` / `node` envelope fields are
still re-stamped per request, so observability headers remain
caller-specific.

The race semantics are explicit: a concurrent `bump()` between
generation snapshot and `store()` causes the stored entry to be
immediately stale — refused by the next `get()` — rather than
served. Coalescing was deliberately not added; only one writer
"wins" the store under contention but both writers stamp the
same generation so freshness is unaffected.

Future follow-ups (mentioned in module doc):
- `?summary=true` mode caching as a separate slot.
- Pagination via `?limit` / `?offset` (skip cache or key by
  page descriptor).
- Same shape can be lifted to `/api/v1/agents/summary` once
  that handler's rebuild cost is measured.
