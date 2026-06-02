# Session summary — root-cause fix for the /api/v1/node full-hang (telemetry blocking the async handler)

## Goal

Reopened-bead follow-up: bd-e32dba's first fix (this session, landed c9df57bd6)
bounded the LATER sub-fetches in `handle_node`, but the operator/reopener proved
the wedge PERSISTS — `/api/v1/node` is a FULL hang (code=000, never returns,
probed to 40s) while `/health`/`/beads`/`/feed` stay fast, and it re-wedges
within ~1min of every fresh daemon. The reopener correctly diagnosed that a
full-hang-that-never-returns means something blocks BEFORE the bounded sections,
or in listener init — outside the CACO_NODE_PROBE_SECTION_TIMEOUT_MS bounds.

## Bead

- `bd-e32dba` (P1 bug, profile senior-dev) — Health watchdog kills healthy
  daemon: /api/v1/node endpoint hangs while agents/beads/feed serve 200.
  Reopened with post-closure evidence (130 self-exits, ~13min cadence,
  re-wedges per fresh daemon, running binary 1.2.1062 pre-first-fix).

## Root cause (this fix)

`handle_node` called `replication::collect_telemetry()` SYNCHRONOUSLY at the top
of the async handler, BEFORE any bounded section. `collect_telemetry()` shells
out to multiple external subprocesses (`netstat`/`ps`/`sysctl`/`df`/`lsof`, disk-IO
sampling) and walks `/proc`. Called directly on the async executor with no
`spawn_blocking` and no timeout, any hung/slow subprocess (a wedged `df`/`lsof`
on a stuck mount, slow `ps`/`netstat` under load — most likely on macOS/ms-mac
where the wedge was observed) blocks the tokio worker thread and makes
`/api/v1/node` a full hang. Sibling endpoints never call telemetry, so they stay
fast — exactly the observed split. It survives restart because every fresh
daemon's first `/api/v1/node` probe re-triggers `collect_telemetry()` and
re-hangs on the same wedged subprocess/mount.

(`read_cacophony_dir_size` was already cached/backgrounded per bd-f9a280, so it
was NOT the culprit; the other `read_*` helpers shell out live.)

## Fix

Wrap `collect_telemetry()` in `tokio::time::timeout(effective_node_probe_section_timeout(),
tokio::task::spawn_blocking(replication::collect_telemetry))`:
- `spawn_blocking` moves the blocking subprocess/proc work OFF the async worker
  thread so it can never stall the handler.
- `timeout` (default 2000ms, same bd-13ae27 knob) bounds it; on timeout or
  join failure we degrade to `NodeTelemetry::default()` so the liveness probe
  ALWAYS returns instead of full-hanging.

This complements the first fix (bounded later sub-fetches) and the watchdog
hardening (corroborate with `/api/v1/feed` before self-exit): the handler now
has no unbounded blocking call before or within it.

## Before / after

- Before: telemetry ran inline on the async path before the bounds; a hung
  subprocess full-hung `/api/v1/node` (code=000) on every probe, re-wedging each
  fresh daemon → watchdog self-exit loop (~13min cadence, 130 self-exits).
- After: telemetry is off-thread and bounded; a hung/slow subprocess yields a
  degraded telemetry section within the section timeout and the handler returns.

## Diff summary

- Code commit: pending final squash SHA from the reintegration receipt.
- Files touched: `crates/caco-daemon/src/lib.rs` (+45/-1).
- Tests: extended `node_endpoint_returns_success_envelope` to assert the
  `load_summary` telemetry section still populates via the bounded
  spawn_blocking path on the healthy path (passing).
- Validation: `cargo check -p caco-daemon` clean, `cargo clippy -p caco-daemon`
  clean, focused test passes (queued lanes); cacophony-fast-tests gate runs at
  reintegration.

## Operator takeaway

The chronic `/api/v1/node` full-hang / watchdog-restart loop has its true root
cause fixed: synchronous blocking telemetry no longer runs on the async handler
path. Nodes pick this up after the binary rebuilds past this commit (binary
rollout is operator territory; the reopener's evidence was from pre-fix 1.2.1062).
Related-but-separate: bd-eead91 / bd-a9419e (helsinki :12100 beads-proxy flap)
remain their own issue.
