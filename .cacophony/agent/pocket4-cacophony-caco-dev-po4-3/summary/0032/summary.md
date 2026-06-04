# S5: suggest persistence + resume + retention + run-state (bd-8441b2)

## Bead
bd-8441b2 — caco suggest S5: persistence under `.cacophony/suggest/<uuid>` +
`--resume`. Slice 5 of the `caco suggest` epic (bd-a84d20). Depends on S3
(generate endpoint, landed `bebbf5942`).

## What changed
New `crates/caco-daemon/src/suggest/store.rs` (registered `pub mod store;`),
plus resume/list handlers + canonical persistence wiring in
`crates/caco-daemon/src/suggest/endpoint.rs`, plus two new routes in the daemon
Router (all listener tables).

### Canonical layout (promotes S3's minimal write)
```text
<root>/suggest/<uuid>/
  context.json        # bounded SuggestContext (S2 shape)
  suggestions.json    # PersistedSet: items + per-item run-state
  runs/               # reserved for S6 /run execution records
```
`store::persist_set` writes this; the generate handler now uses it (replacing the
S3 minimal `set.json`).

### Resume + list endpoints
- `GET /api/v1/suggest/<uuid>` (`handle_suggest_get`) — returns the persisted
  set's items + context. **No new LLM call.** 404 envelope when absent.
- `GET /api/v1/suggest/list` (`handle_suggest_list`) — all locally-persisted
  sets, newest-first, deduped by uuid (dir name = uuid).
- Route ordering places `/list` before `/{uuid}` so it cannot be captured as a
  uuid.

### Scope pointer + most-recent-for-scope (for `caco suggest --resume`)
- `scope_pointer_key` → `project:<id>` | `node:<name>` | `global` (project wins
  over node, matching gather precedence).
- `most_recent_for_scope(sets, scope)` — pure newest-by-`created_at` pick used by
  `--resume` with no explicit uuid. (The `caco suggest --resume` CLI command is
  caco-cli-side; S5 provides the daemon read surface it uses.)

### Bounded retention
- `prune_decisions` (pure: count + age) and `run_retention` (fs sweep) with
  `DEFAULT_SUGGEST_RETENTION_COUNT` (50) + `DEFAULT_SUGGEST_RETENTION_AGE_SECS`
  (14d). Swept best-effort after each generate.

### Run-state types + full-state summary types
- `ItemRunState { run_count, last_run_status, last_run_at }` (S6 updates these on
  `/run`; S5 defines + persists + carries them).
- `PersistedItem` / `PersistedSet` (on-disk shape).
- `SuggestSetSummary` / `SuggestItemSummary` + `FULL_STATE_MAX_SUGGEST_SETS` (10)
  — the bounded, log-free types for full-state sync, ready for the follow-up.

## Scope split — full-state replication → bd-2bff13 (S5b)
Harry's epic refinement (5) wants run-state convergence via a bounded
`suggest_sets` field in `FullStateDump` (mirroring `choice_history`), plus a
`SuggestSetCreated` feed event (refinement 3). That touches 10+ `FullStateDump`
construction sites + the ingest/merge path + a new `EventType` variant — a
cross-cutting replication-hot-path change. To keep this slice reviewable and not
destabilize replication, S5 lands the persistence/resume/retention/run-state
**core** (which fully unblocks S6) and the replication wiring is split into
**bd-2bff13 (S5b)**, blocked on this S5, with the bounded summary types already
defined here so S5b is purely wiring.

## Tests
9 new store tests (26 total in the suggest module), all green via the daemon
queue (`RUST_MIN_STACK=33554432 cargo test -p caco-daemon --lib suggest:: -- --test-threads=1`):
- `from_generated_starts_unrun`
- `persist_load_roundtrip` (context + items + reserved runs/ dir)
- `list_sets_newest_first`
- `scope_pointer_precedence` (project wins over node)
- `most_recent_for_scope_filters_and_picks_newest`
- `prune_decisions_count_and_age`
- `run_retention_removes_old_dirs`
- `rfc3339_parse_fallback_is_zero`
- `summary_from_persisted_carries_run_state_no_logs`

## SPEC areas
SPEC 6.x daemon endpoint + on-disk runtime-state surface (append-only files +
inspectable JSON); epic bd-a84d20 (resume = no new LLM call; node-local;
read-only list). No SPEC contract change — additive.

## Diff summary
New store module + resume/list handlers + canonical persist wiring + 2 routes
(×4 tables). Final landed squash SHA per the reintegration receipt.
