# Session summary — bd-fa0603: per-peer beads divergence doctor sensor

## Goal

Defense-in-depth from the bd-cf99b7 incident: a destructive reconciler
truncated cacophony beads from 2775 to 113 and the per-peer divergence
(helsinki=35, pocket4=117, ms-mac=5 — 70%+) went undetected for 4h35m
because nothing automated compared per-peer counts.

## Bead(s)

- `bd-fa0603` — Per-peer bead-count divergence alarm (caco doctor +
  project-health) — alert when any two peers differ >10% for >5min,
  root from bd-cf99b7

## Before state

- `caco doctor` had no sensor that compared per-peer bead counts.
- The existing `/api/v1/projects/<p>/beads/stats` endpoint forwards to
  the authoritative beads primary, so peer-side queries all returned
  the same authoritative count and could not be used to detect
  divergence.
- `caco-doctor` had 11 check categories (config, auth, runtime,
  storage, version, etc.) but nothing in the `beads` area beyond
  journal-tail malformed-line counting.

## After state

### Daemon: new local-count endpoint (criterion 4)

- `crates/caco-daemon/src/beads.rs`: new `handle_local_bead_count`
  handler that reads `cached_beads_store(state, project)` and returns
  `{available: true, visible, by_status}` for available peers, or
  `{available: false, reason}` for sparse-checkout / store-error peers
  (so callers exclude rather than zero-count).
- `crates/caco-daemon/src/lib.rs`: route registered on all four router
  variants (`/api/v1/projects/{project}/beads/local-count`).
- Unlike `/beads/stats` this never proxies — every peer reports its
  own local store count, which is exactly what divergence detection
  needs.

### CLI doctor sensor (criterion 1)

- `crates/caco-cli/src/lib.rs`: `check_beads_peer_divergence` queries
  every configured peer's local-count endpoint and pushes one
  `DoctorCheck` per project. Pure helper `classify_beads_divergence`
  factored out for testability.
- Per-peer URL: `127.0.0.1:cluster_port` for the local node,
  `node.host:cluster_port` for peers; cluster_port defaults to 12100.
- 3-second per-peer HTTP timeout so the doctor sweep doesn't stall
  on an unreachable peer.
- Threshold: `(max - min) / max > 10%` → `error`.

### Tests (criterion 5)

- 6 unit tests for `classify_beads_divergence`:
  - `bd_cf99b7_incident_signature` — exact incident counts
    {35, 117, 5} → error, divergence ≈ 0.957.
  - `steady_state_within_threshold` — {2820, 2810, 2815} → ok,
    <1% divergence.
  - `at_threshold_boundary` — exactly 10% is not >threshold (ok),
    just over 10% trips error.
  - `single_peer_is_ok` — can't divergence-check with one peer.
  - `empty_is_ok` — no peers reported.
  - `all_zero_no_division_panic` — fresh project, no division-by-zero.
- `cargo test-small`: 57 pass.
- `cargo clippy --workspace --all-targets -- -D warnings`: clean.

## Diff summary

- 1 commit, 3 files (`crates/caco-daemon/src/beads.rs`,
  `crates/caco-daemon/src/lib.rs`, `crates/caco-cli/src/lib.rs`).
- Net: +346 lines.

## Deferred to follow-up beads

- Criterion 2: project-health agent gets a periodic 5-min check that
  files a P0 bead on sustained divergence (with dedup against existing
  open divergence beads).
- Criterion 3: doctor `recovery hints` line for divergence-detected.
- Criterion 6: integration test against the live mesh that asserts
  zero divergence in steady state.
- The bead's >5min sustained-divergence requirement: the doctor runs
  on demand so this commit reports instantaneous divergence; the
  5-min window enforcement belongs in the project-health periodic
  check where the sensor can be polled and the result rate-limited.

## Operator-takeaway

After-rollout, `caco doctor` will surface a `beads / peer divergence
'<project>'` check per configured project. In steady state it shows
`ok max=2820 min=2810 divergence=0.4% peers=[ms-mac=2820, ...]`. On
a bd-cf99b7-class incident it shows `error` and surfaces the per-peer
counts so the operator can immediately identify which peer is out of
sync.
