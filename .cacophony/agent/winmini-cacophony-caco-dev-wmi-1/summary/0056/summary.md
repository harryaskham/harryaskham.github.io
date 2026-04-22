# Session summary — bd-dfc91a: doctor honest about local-stale-but-healthy daemon

## Goal

Stop `caco doctor` from emitting an `error` + "Is the
daemon running?" hint for the *local* node when the
on-disk daemon_state snapshot is stale but the local
daemon is actively serving requests.

## Bead(s)

- `bd-dfc91a` — own bead filed after observing the
  misleading error during this session. Closed.

## Before state

- `caco doctor` on winmini (where the daemon is
  demonstrably alive — every other caco command works)
  reported:
  ```
  state:
    ✗ error  daemon_state 'winmini'  6362s ago
  recovery hints:
    → Daemon state for node 'winmini' is stale. Is the
      daemon running?
  ```
- The check only inspected the snapshot file mtime, not
  whether the daemon process was actually alive on
  /api/v1/health.

## After state

- For the local node specifically, the freshness check
  now does a 2s probe of
  `http://127.0.0.1:<port>/api/v1/health`. Port resolved
  from `services.caco_daemon[node==local].port` (default
  12100 fallback).
- If `is_local && local_health_alive`, the stale snapshot
  is reported as **info** (not error), with detail:
  `Ns ago (snapshot stale; local daemon healthy via /health)`.
  The misleading "Is the daemon running?" hint is
  suppressed.
- Verified live: `daemon_state 'winmini'` now reads
  `info  6887s ago (snapshot stale; local daemon healthy
  via /health)` while `daemon_state 'beelink'` (3s ago)
  still reads `ok` as before. Other-node stale snapshots
  remain `error` (the operator should still know about a
  truly dead peer).

## Diff summary

- 1 file touched, +44 / −5:
  - `crates/caco-cli/src/lib.rs`: dispatch_doctor section
    10 (daemon_state freshness): added is_local +
    /health probe + downgrade-to-info branch.

## Verification

- `cargo build -p caco-cli`: clean.
- `./target/debug/caco doctor` against the running
  winmini daemon now shows `info` for the local node,
  `ok`/`warning`/`error` unchanged for other nodes.

## Operator-takeaway

Family with bd-126b99 / bd-a403a1 / bd-30fbfb / bd-2886bb
(CLI honesty pass) — checks should report the actual
state observed, not the worst case implied by a single
stale file. Real fix for the snapshot-not-refreshing
itself is a separate daemon-side investigation; this
mitigation stops the noise from reaching operators in
the meantime.
