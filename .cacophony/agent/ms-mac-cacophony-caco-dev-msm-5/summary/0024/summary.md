# Session summary 0024 — bd-9d5be2: async checkout-size cache hydration

## Goal

Reduce GET /api/v1/agents latency from 3–5s to sub-second under
30+ agent load by removing synchronous dir_size walks from the
request path.

## Bead(s)

- `bd-9d5be2` slice 1 — checkout-size cache hydration off the hot path.

## Before state

- `scan_agents_dir_filtered` calls `ensure_cached_checkout_size`
  per agent. When the cache file is missing (newly-discovered
  agent), `ensure_cached_checkout_size` did a synchronous
  recursive `dir_size` walk inline.
- `list_all_with_disk_refresh` (called by `handle_agents_list`)
  inherited this cost, blocking the request for hundreds of ms
  per agent × 30 agents = 3–5s total.

## After state

- `ensure_cached_checkout_size` now writes a placeholder `0` cache
  file synchronously (so subsequent `scan_agents_dir` calls don't
  re-spawn) and dispatches the real recursive walk to an OS thread
  via `std::thread::spawn`.
- First-list callers see `checkout_size_bytes: 0` for any agent
  discovered for the first time; subsequent list calls see the
  real value once the background walk completes.
- Lifecycle transitions (start/stop/prune at lifecycle.rs:1357,
  1850, 2341, 3434) continue to refresh the cache eagerly, so
  the freshness gap only spans the disk-hydration warm-up path.

## Diff summary

- Commit: `2b676afb`.
- Files (1): `crates/caco-daemon/src/agent/health.rs`.
- `cargo build -p caco-daemon` + clippy: clean.

## Operator-takeaway

`caco agent list` and TUI/web first-paint should now return in
sub-second time even with 30+ agents. The first call after a
daemon restart may briefly show `size=0` for any agents whose
cache hasn't been populated yet — this clears within a few
seconds as the background threads finish.

Other latency improvements (response cache invalidated on
state-change, ?summary mode that skips heavy fields, pagination)
are deferred to bd-9d5be2 follow-ups.
