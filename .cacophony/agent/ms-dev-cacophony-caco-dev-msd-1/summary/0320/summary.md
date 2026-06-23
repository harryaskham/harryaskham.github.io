# Session summary — bd-fbdd22 slice 1: bound the agents-list cache-miss rebuild (no more blank reads under registry-lock contention)

## Goal

Re-engage on real daemon work once host load recovered from the overnight nix-update storm, and land a focused, well-tested first slice of the registry-read-contention reliability bug (bd-fbdd22) — the one behind the blank `caco agent list` / `caco ps` / TUI / caco-web reads the whole fleet (including this agent) hit during the load surge. Make the fix populate-not-blank and diagnosable, mirroring the established bounded-read precedent rather than rewriting the registry lock.

## Bead(s)

- `bd-fbdd22` — [daemon-reliability] Registry-read contention: list/ps/nudge/mesh aggregate reads blank under sustained registry-lock load (registry-side sibling to bd-057f2e Slice A). **Slice 1 only; bead stays open** for the remaining mesh + nudge-wake slices.
- Context this session also parked the long-churned `bd-9b88a4` behind new prereq draft `bd-b564e5`, and filed reflect-session draft `bd-d7f05b` (auto-claim skip coordinate-first).

## Before state

- Failing tests: none (pre-existing main green).
- `handle_agents_list` (`/api/v1/agents`): on a bd-7a1ca5 cache MISS, the rebuild called `state.agents.list_all_with_disk_refresh().await` with NO timeout. Under sustained registry-write contention the generation bumps on every lifecycle write → frequent cache misses → the rebuild took the contended agent-manager lock and blocked for tens of seconds, returning blank/empty agent lists. `caco ps` calls `/api/v1/agents` (+ `/api/v1/node`, already bd-13ae27-bounded), so it blanked too.
- Callers could not distinguish a lock-starved blank from a genuinely empty inventory (msd-4's overnight diagnostic).

## After state

- Failing tests: none. New tests 3/3 green (run via the queue since `caco-daemon` is excluded from the `test-small` gate); `cargo clippy -p caco-daemon` clean (0/0).
- `handle_agents_list` bounds the cache-miss rebuild with `effective_agents_list_rebuild_timeout()` (`CACO_AGENTS_LIST_REBUILD_TIMEOUT_SECS`, default 3s). On timeout it serves the last cached agent list marked stale (`served_stale: true`) when available — populate, not blank — else a structured degraded-empty body (`degraded: true`, `degraded_reason: "registry_lock_contended"`) so callers can tell lock-starvation from an empty inventory.
- New `AgentsListCache::peek_last_body()` returns the last stored body ignoring generation (the serve-stale primitive).

## Diff summary

- Code/content commits: `14b890af13` (pending final squash SHA from the reintegration receipt).
- Summary artefact commit: intentionally omitted (must not self-reference its own mutable SHA).
- Files touched: `crates/caco-daemon/src/agents_list_cache.rs` (+`peek_last_body` + unit test), `crates/caco-daemon/src/lib.rs` (timeout const + helper, two pure degraded-body helpers, bounded rebuild wrap in `handle_agents_list`, two pure-helper unit tests).
- Tests: +3 (peek_last_body stale-recovery; degraded-marker stamping + field preservation; degraded-empty not-silently-blank). Flipped 0.
- Behavioural delta: `/api/v1/agents` (and therefore `caco agent list` / `caco ps`) now returns the last-known agent list marked stale, or an explicit degraded marker, instead of blocking/blanking when the agent-manager lock is contended past 3s. Additive JSON fields only (backward-compatible).

## Operator-takeaway

The fleet's intermittent "blank agent list / ps under load" was the `/api/v1/agents` cache-MISS rebuild hitting the contended agent-manager lock unbounded; the bd-7a1ca5 cache only protected repeat reads at the same generation, and sustained write load keeps bumping the generation. This slice bounds that rebuild and serves the last-known list stale-marked (or an explicit degraded marker) instead of blanking — same pattern bd-13ae27 used for `/api/v1/node`. bd-fbdd22 stays open: the mesh aggregate read (dynamic_registry) and the nudge functional-wake-under-contention (bd-2ba5ed fix(b)) are still uncovered and are the natural next slices.
