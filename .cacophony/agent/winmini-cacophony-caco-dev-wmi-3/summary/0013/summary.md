# Session summary — bd-df4573 /api/v1/node telemetry hang (leaked netstat children)

## Goal

Fix the P1 daemon bug where `/api/v1/node` (the liveness/connect probe the macOS
companion app and `caco status` depend on) returns HTTP 000 / full hang on macOS
under load, breaking the operator's app connection. Implement the operator's
preferred two-part fix: bound + kill the telemetry subprocesses AND split
telemetry off the liveness hot path, with strong tests.

## Bead(s)

- `bd-df4573` — /api/v1/node hangs (HTTP 000) on macOS: collect_telemetry leaks
  hung `netstat -ib` children, saturating the blocking pool (incomplete
  bd-e32dba fix). P1, labels broken-on-main, daemon, macos-impact, replication.

## Before state

- Failing tests: none directly; live operator-impacting runtime hang on ms-mac.
- Context: `replication::collect_telemetry()` shells out to `netstat -ib`,
  `sysctl`, `vm_stat`, `ps`, `df` via blocking `std::process::Command::output()`
  with no timeout and no child kill. On macOS under load `netstat -ib` hangs;
  each `/api/v1/node` probe leaked a stuck child (7+ observed) and parked a
  blocking thread, saturating the spawn_blocking pool so the probe returned 000.
  bd-e32dba bounded the handler wait but never killed the child.

## After state

- Failing tests: none. `cargo test -p caco-daemon --lib replication::tests`:
  167 passed / 0 failed. `cargo clippy -p caco-daemon -- -D warnings`: clean.
- Context: every telemetry shell-out now routes through a bounded helper that
  spawns the child as its own process-group leader and SIGKILLs the whole group
  on a 2s timeout (mirroring the bd-5dfb6e/bd-e4e93c `BoundedCommandOutput`
  pattern in `agent/health.rs`), so a hung tool can no longer leak a child or
  saturate the pool. Telemetry is served from a short-TTL (3s) process-global
  cache, and `/api/v1/node` uses a new subprocess-free liveness path
  (`collect_telemetry_for_liveness`) that returns cached/last-known telemetry
  immediately and warms the cache in the background — so the connect probe is
  always near-instant and never blocks on telemetry.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted (no self-reference).
- Files touched:
  - `crates/caco-daemon/src/replication.rs` — add `bounded_telemetry_output`
    (process-group-leader spawn + bounded wait + group SIGKILL) and
    `kill_telemetry_process_group`; route `netstat`/`sysctl`/`vm_stat`/`ps`/`df`
    through it; add `TELEMETRY_CACHE` + `TELEMETRY_CACHE_TTL`; rename the raw
    collector to `collect_telemetry_uncached`; make `collect_telemetry` a cached
    wrapper; add `collect_telemetry_for_liveness` (never shells out
    synchronously); new tests (hung-child kill, fast command, cache identity,
    liveness fast-path); point the two rate-sampling tests at the uncached path.
  - `crates/caco-daemon/src/lib.rs` — `handle_node` now calls
    `collect_telemetry_for_liveness()` instead of the blocking collector.
- Tests: +4 (bounded kill, fast command, cache, liveness) / -0 / 2 retargeted to
  uncached.
- Behavioural delta: `/api/v1/node` stays responsive even when a telemetry
  subprocess hangs; no telemetry child outlives its 2s bound; the liveness path
  never shells out.

## Operator-takeaway

The macOS "app can't reach daemon" hang was a leaked-subprocess saturation, not
load: an unbounded `netstat -ib` (and siblings) had no timeout/child-kill, so
each probe stranded a child and a blocking thread until the pool was exhausted.
The durable cure is two-layered: bound+group-kill every telemetry shell-out so
nothing can leak, and keep the liveness/connect probe entirely off the
subprocess path via a short-TTL cache + background-warmed liveness accessor. Any
future telemetry tool added to `collect_telemetry_uncached` should go through
`bounded_telemetry_output`, never raw `.output()`.
