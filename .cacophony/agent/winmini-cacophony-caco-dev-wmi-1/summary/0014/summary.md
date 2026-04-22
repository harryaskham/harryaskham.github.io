# Session summary — Integration tests for /api/v1/agents cache (bd-63e0c1)

## Goal

bd-7a1ca5 added the `/api/v1/agents` JSON response cache with
9 unit tests covering the cache primitive in isolation
(`AgentsListCache::{new, generation, bump, get, store, clear}`).
It did not have an integration test verifying the
wired-into-`DaemonState` behaviour. This adds end-to-end tests
that drive the actual HTTP endpoint via `local_router`.

## Bead(s)

- `bd-63e0c1` — [bd-7a1ca5 follow-up] Integration test for
  `/api/v1/agents` response cache invalidation across lifecycle
  events (filed by this agent during this session)

## Before state

- 9 cache-primitive unit tests (`agents_list_cache::tests::*`).
- Zero handler-level tests.
- The wiring (`set_agents_list_cache`, `bump_agents_list_cache`,
  `set_state` calling `bump_agents_list_cache`) was untested
  end-to-end — it relied on grep-level review.

## After state

- 12 cache tests total (9 primitive + 3 new integration).
- The full request → cache hit/miss → state mutation → cache
  invalidation → rebuilt response loop is now defended.

## Diff summary

- Files touched (+226 / −2):
  - `crates/caco-daemon/src/lib.rs`: 3 new tests + helper
    `fetch_agents_list(app)`.
  - `crates/caco-daemon/src/agent/mod.rs`: new
    `inner_for_test()` `#[doc(hidden)] pub` accessor returning
    `&Arc<Mutex<AgentManagerInner>>` so the lib.rs tests can
    inject synthetic agent rows without paying the cost of a
    full `create()` pipeline (resolved profiles, checkout
    dirs, tmux setup).
  - `crates/caco-daemon/src/ui_stream.rs`: 2 duplicate
    `tmux_history_*: None` lines stripped (5th wave this week
    of broken-on-main fixture churn).

### Tests added

1. **`agents_list_cache_serves_identical_body_within_generation`**
   - Two `GET /api/v1/agents` back-to-back without state
     mutation. Asserts:
     - `cache.generation()` does not advance.
     - `body1["data"] == body2["data"]` (envelope `request_id`
       and `node` fields are correctly re-stamped per request;
       only the inner `data` payload is cached).

2. **`agents_list_cache_invalidated_by_explicit_bump`**
   - GET populates cache at gen N. Direct `cache.bump()`
     (mirrors what `AgentManager` does internally from
     `set_state` / `create` / `discard` / `prune`). Asserts:
     - generation advanced strictly,
     - `cache.get()` returns `None` immediately post-bump,
     - subsequent GET rebuilds and `cache.get()` returns
       `Some` at the new generation.

3. **`agents_list_cache_invalidated_by_real_set_state_call`**
   - Production-shape test: wires `AgentManager` via
     `set_agents_list_cache`, injects a synthetic agent into
     the inner map via the new `inner_for_test()` helper,
     GETs once to populate cache, calls
     `set_state("agent-cache-test", AgentState::Running)`.
     Asserts:
     - cache generation advanced (proving the wired
       `bump_agents_list_cache` hook fires from inside
       `set_state`),
     - cache is stale immediately after,
     - subsequent GET rebuilds and the new agent's state in
       the rebuilt list reflects `"running"`.

## Embedded artefacts

(none — pure test additions + one `#[doc(hidden)]` accessor)

## Operator-takeaway

The `bd-7a1ca5` cache wiring is now defended end-to-end:

- A future change that drops the `set_agents_list_cache` call
  in `pub async fn run()` will fail test #3 (set_state won't
  bump because the hook is unwired in production).
- A future change that removes the `bump_agents_list_cache`
  call from `set_state` will fail test #3 in a different way
  (generation won't advance).
- A future change that makes `handle_agents_list` cache the
  envelope (instead of just the inner data) will fail test #1
  (request_ids would no longer be unique per request).
- A future change that breaks the post-bump rebuild path will
  fail test #2.

The new `inner_for_test()` accessor is `#[doc(hidden)] pub`,
not `pub(crate)`, because integration tests in
`crates/caco-daemon/tests/` are external consumers and may
need it for future test slices (no immediate consumer there).

## Drive-by

Yet another (5th) round of `tmux_history_*` duplicate fixture
lines this week. Stripped 2 lines in `ui_stream.rs`. Already
filed as a known pattern under `bd-29bf2b` (merge-queue mixin
upgrade to add `cargo test --lib --workspace` to the gate).
