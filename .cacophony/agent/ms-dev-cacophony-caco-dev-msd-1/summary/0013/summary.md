# Session summary — bd-75662c daemon shutdown drain

## Goal

Land bd-75662c, the bd-3bcac8 follow-up that teaches the daemon
to actually drain in-flight requests on graceful shutdown
instead of just sleeping 500ms and exiting. Operators using
`caco daemon shutdown` for safe binary swaps should see new
requests bounced with `503 + Retry-After` immediately, while
in-flight handlers get up to 30s to finish before forced exit.

## Bead(s)

- `bd-75662c` — [bd-3bcac8 follow-up] daemon shutdown drain
  logic + 503 Retry-After during drain window.

## Diff summary

**New module `crates/caco-daemon/src/drain.rs`** (~190 lines
incl. tests):
- `DrainState` with `AtomicBool draining`, `AtomicUsize
  inflight`, `Mutex<Option<Instant>> started_at`,
  `Mutex<Duration> drain_timeout`.
- `begin_drain()` — idempotent flip; returns `true` exactly
  once so the winning caller spawns the drain task.
- `track_inflight()` — RAII `InflightGuard` that
  increments/decrements the counter at middleware enter/exit.
- `is_draining`, `drain_started_at`, `drain_timeout`,
  `set_drain_timeout`, `inflight` accessors.
- Constants: `DEFAULT_DRAIN_TIMEOUT = 30s`,
  `DRAIN_RETRY_AFTER_SECS = 5`.
- 4 unit tests (idempotent flip, inflight guard, timeout
  override, retry-after sanity).

**`crates/caco-daemon/src/lib.rs`:**

1. Added `pub drain: Arc<drain::DrainState>` to `DaemonState`
   and threaded the field through 14 constructor sites
   (production + test fixtures).
2. New `drain_middleware`: layered between
   `request_timeout_middleware` and `panic_catch_middleware`.
   When draining, returns 503 with `Retry-After: 5` for every
   path **except** `/api/v1/daemon/shutdown` (idempotency +
   status visibility for re-issued shutdowns). Otherwise wraps
   the handler in an `InflightGuard` so the shutdown task can
   poll for completion.
3. Rewrote `handle_daemon_shutdown` (was bd-3bcac8 slice 1
   500ms-sleep-and-exit). New flow:
   - `begin_drain()` flips the flag (idempotent — re-issued
     shutdowns short-circuit).
   - Background task sleeps 500ms (response flush), then polls
     `inflight()` every 50ms until ≤1 (just our own slot) or
     `drain_timeout` (30s default) elapses, then `exit(0)`.
   - Structured `eprintln!` at each branch (drain complete,
     drain timeout, re-requested while draining).
4. Two new `#[tokio::test]` integration tests in the existing
   `mod tests` block:
   - `drain_middleware_returns_503_with_retry_after_when_draining`
     drives a `GET /api/v1/node` through `local_router` first
     before draining (200), then flips the flag and asserts
     503 + `Retry-After: 5`.
   - `drain_middleware_lets_shutdown_endpoint_through_during_drain`
     pre-flips drain (so the real exit task is a no-op) and
     asserts a second `POST /api/v1/daemon/shutdown` returns
     200 with `accepted=true`.

**`crates/caco-daemon/src/ui_stream.rs`:** drive-by — added
the two missing `tmux_history_limit: None` /
`tmux_history_size: None` fields to a single `AgentSnapshot`
test fixture (the `agent_snapshot_without_checkout_path_round_trips`
test) that escaped both my fix-forward (msd-1 bd-1c0bdd cycle)
and msd-2's bd-c36993 sweep.

## Before state

- `POST /api/v1/daemon/shutdown` returned 200, slept 500ms,
  called `exit(0)`. In-flight requests were dropped on the
  floor; new requests during the 500ms window were accepted
  and then orphaned.
- No way for clients to distinguish "daemon is leaving" from a
  hard crash.

## After state

- Same 200 response shape (no protocol break).
- Drain flag is set immediately; new requests get a structured
  `daemon_draining` 503 with `Retry-After: 5` so clients
  (`caco-cli`, peer daemons, sidecars) can back off cleanly.
- Shutdown endpoint stays reachable for idempotent re-issue.
- In-flight handlers get up to 30s to complete before forced
  exit.
- Telemetry: drain start/complete/timeout each logged with
  `bd-75662c:` prefix and elapsed-ms / inflight count for
  postmortem.

## Notes / verification

- `cargo test-small` 56/56 green.
- `cargo test -p caco-daemon --lib drain` 9/9 green
  (4 module unit tests + 2 new middleware tests + 3 unrelated
  pre-existing tests with `drain` in the name).
- One pre-existing `cleanup_checkout_processes_kills_orphans_and_reports_count`
  failure — also reported by msd-4 in summary 0016, unrelated
  to drain (orphan-process kill counting test). Not a
  this-cycle regression.
- `caco daemon shutdown` CLI surface (bd-3bcac8) needs no
  change — it just POSTs to the same endpoint and gets the
  same response shape.

## Out of scope

- DB fsync / WAL checkpoint on shutdown — daemon `store` is
  `Arc<tokio::sync::Mutex<DaemonStore>>` and `DaemonStore`
  exposes no checkpoint method. Beads stores already have a
  60s WAL-checkpoint loop (lib.rs:6196) so the shutdown path
  rarely loses meaningful WAL volume in practice. A future
  bead can add `DaemonStore::flush_for_shutdown()` if
  postmortems show stale state.
- Per-request `?timeout_secs=N` override — `set_drain_timeout`
  is in place but no API exposes it yet. Operators can rely on
  the 30s default.
- Drain status endpoint (`GET /api/v1/daemon/drain` returning
  inflight count + elapsed) — defer until an operator workflow
  demands it; the structured `eprintln!` covers the immediate
  postmortem need.

## Operator-takeaway

`caco daemon shutdown` now actually drains. New requests get a
clean 503 + Retry-After during the wind-down window;
in-flight handlers get up to 30s to complete before forced
exit. Same protocol response shape, no client breakage. Pairs
with bd-3bcac8 (parent: scaffold + CLI), bd-732406 (reattach),
bd-65813b (crash log).
