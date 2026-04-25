# Session summary — per-state agent checkout disk telemetry

## Goal

Complete the residual work on bd-f9419a: split the `agent_checkouts`
disk-growth category into per-agent-state sub-sums (running,
completed, stopped, failed, other) so cluster-ctrl and `caco doctor`
can distinguish healthy active-agent growth from purgeable
completed/stopped/failed checkout bloat without manual verification.

## Bead(s)

- `bd-f9419a` — Disk-growth telemetry should distinguish running-
  checkout / completed / stopped / cargo-cache / db growth so
  cluster-ctrl can act with precision

## Before state

- `caco doctor` disk breakdown reported a single `agent_checkouts`
  aggregate plus `agent_cargo_targets` sub-sum. No per-state split.
- A `// FOLLOWUP(bd-f9419a)` comment in `disk_breakdown.rs` deferred
  the split, noting it required daemon state correlation.
- Module docs stated the per-state split was "left to a follow-up
  bead."

## After state

- Per-state sub-categories emitted: `agent_checkouts_running`,
  `agent_checkouts_completed`, `agent_checkouts_stopped`,
  `agent_checkouts_failed`, `agent_checkouts_other`. Each is a
  child of `agent_checkouts` (tagged with `parent` field), emitted
  only when non-zero.
- Implementation reads `agent.json` directly from the filesystem —
  no daemon round-trip — so it works even when the daemon is down.
- `other` category calculation updated to exclude all sub-sum
  categories (was only excluding `agent_cargo_targets`).
- Module docs updated to reflect the shipped state.
- 17/17 disk_breakdown tests pass; cargo test-small 252/252; clippy clean.

## Diff summary

- Commit: `8f599ccde`
- Files touched: `crates/caco-cli/src/disk_breakdown.rs` (+187 / -42)
- Tests: +3 new (per-state split, read_agent_state helpers), 1
  updated (total-matches-root-du uses parent-based sub-sum exclusion).
- Behavioural delta: `caco doctor` disk breakdown now shows per-state
  checkout sizes. JSON consumers see new categories in the
  `categories` array with `parent: "agent_checkouts"`.

## Operator-takeaway

The per-state split reads persisted `agent.json` alongside each
checkout — no daemon required. This means `caco doctor` gives
accurate state-classified disk breakdown even during daemon restarts
or on nodes where the daemon has crashed. Cluster-ctrl can now
distinguish "20 GiB growth from 5 actively-compiling agents" from
"20 GiB of completed checkouts nobody purged" without shelling into
each node.
