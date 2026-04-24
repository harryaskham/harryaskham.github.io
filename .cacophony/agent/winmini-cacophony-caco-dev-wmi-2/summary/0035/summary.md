# Session summary — bd-6b62ea doctor shows disk usage by agent state

## Goal

Take another contained child slice under `bd-f9419a`: surface the daemon’s already-existing `disk_usage_by_state_bytes` data inside `caco doctor`, so operators can see running/completed/stopped checkout bytes in the doctor output without waiting for a broader disk-telemetry redesign.

## Bead(s)

- `bd-6b62ea` — Show daemon disk usage by agent state in doctor
- (parent context: `bd-f9419a` remains open; this ship covers only the doctor read-surface slice, not the full state-aware filesystem telemetry redesign)

## Before state

- `caco doctor` already rendered the bd-f9419a filesystem category breakdown from `disk_breakdown.rs`.
- `caco-daemon` already exposed `disk_usage_by_state_bytes` in `/api/v1/agents/summary`.
- `caco-cli` already consumed that field in the fleet summary path.
- But `dispatch_doctor(...)` did not surface those per-state checkout bytes at all, even though they were already available from the daemon.

## After state

- `dispatch_doctor(...)` now fetches `/api/v1/agents/summary` once and reuses that parsed response.
- The existing stuck-agents check now consumes the shared parsed summary instead of doing its own one-off fetch/parse path.
- After the filesystem category checks, `caco doctor` now emits additional `storage` checks for daemon-reported checkout bytes by agent state, e.g.:
  - `disk by agent state: running`
  - `disk by agent state: completed`
- This is intentionally a read-surface slice only:
  - no daemon storage/schema changes
  - no change to `disk_breakdown.rs` category semantics
  - just visibility of already-computed daemon data inside doctor

## Diff summary

- Commit: `67f77fb64` — `bd-6b62ea: show doctor disk usage by state`
- Files touched:
  - `crates/caco-cli/src/lib.rs`
- Diff vs current `origin/main`:
  - `crates/caco-cli/src/lib.rs` — +148 / -60
- Behavioural delta:
  - `caco doctor` now shows daemon-reported disk bytes split by agent state
  - doctor internally reuses one parsed agents-summary payload for multiple checks instead of parsing it twice
- Validation:
  - `cargo test -p caco-cli tests::doctor_surfaces_disk_usage_by_agent_state_bd_6b62ea -- --exact --nocapture`
  - `cargo build -p caco-cli`
  - `cargo clippy -p caco-cli --all-targets --no-deps -- -D warnings`

## Operator-takeaway

This is a real improvement to the disk-growth story, but still intentionally narrow. Operators running `caco doctor` can now see the daemon’s split of checkout disk by agent state without hand-calling other surfaces, which makes `bd-f9419a` materially better even though the broader state-aware filesystem telemetry work remains open.