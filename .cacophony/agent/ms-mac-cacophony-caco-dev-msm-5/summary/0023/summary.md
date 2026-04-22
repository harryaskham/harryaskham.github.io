# Session summary 0023 — bd-fcc343: doctor agent_cargo_targets thresholds

## Goal

Make worker-checkout cargo target/ disk pressure visible in
`caco doctor` before the host-disk threshold trips, addressing the
bd-fcc343 incident where total cargo bytes hit ~50–90 GiB.

## Bead(s)

- `bd-fcc343` slice 1 — observability threshold.

## Before state

- `disk_breakdown` already split cargo target/ dirs into
  `agent_cargo_targets` per-category (bd-f9419a), but doctor
  emitted all categories as `ok` regardless of magnitude.
- Operators saw cargo bytes only inside the global host-disk
  warning, by which time burndown had nearly tripped cluster-ctrl.

## After state

- `agent_cargo_targets ≥ 30 GiB` → check status `warning` + hint:
  "approaching cluster-ctrl disk-warning threshold; run
  `caco prune --target` for stopped agents."
- `agent_cargo_targets ≥ 60 GiB` → check status `error` + hint
  citing bd-fcc343 for the shared-target-dir design options.
- Other categories unchanged (delta-since-sample provides rate).

## Diff summary

- Commit: `4d8798b0`.
- Files (1): `crates/caco-cli/src/lib.rs`.
- `cargo build -p caco-cli` + clippy: clean.

## Operator-takeaway

Run `caco doctor` after a burndown — if it flags
`storage / disk: agent_cargo_targets` as warning/error, run
`caco prune --target` or `cargo clean` in idle worker checkouts.
The structural fix (shared CARGO_TARGET_DIR / sccache / btrfs
reflink clones) is bd-fcc343 follow-up territory and needs
operator design — the warning at least makes the threshold
crossable without waiting for the global host-disk alarm.
