# Session summary — power/battery telemetry + doctor power-down alert (bd-ecd49b)

## Goal

Add per-node power/battery status to mesh node telemetry and a `caco doctor`
power-down operator alert, so the fleet is warned before a machine dies on
battery — motivated by the ms-mac battery-depletion crash that took the daemon
+ all its agents down with no warning.

## Bead

- `bd-ecd49b` (P2 feature) — power/battery telemetry + doctor power-down alert.
- bd-f8d5e1 (location telemetry) depends on this; the telemetry+doctor plumbing
  was built generically for reuse.

## Before state

- Node telemetry (`replication::NodeTelemetry`, on `/api/v1/node` + cross-node
  snapshot) carried load/mem/cpu/temp/net/disk but NOT power, so the fleet had
  no visibility into a node about to lose power, and `caco doctor` had no power
  row or alert.

## After state

- **Slice 1 (telemetry, landed 1f770e911a):** `power: Option<PowerStatus>` on
  `NodeTelemetry`, collected platform-natively (Linux `/sys/class/power_supply`,
  macOS bounded `pmset -g batt`); AC-only/server nodes report
  `on_ac=true/has_battery=false`. Power rides the existing cross-node daemon
  snapshot (so peers see each node's power) and `/api/v1/nodes`. Pure tested
  parsers + the `power_alert_level` predicate (Critical only on on-battery AND
  discharging AND at/below the low threshold).
- **Slice 2 (doctor, this PR):** `caco doctor` reads `/api/v1/nodes`, adds a
  per-node power-health row, and raises a power-down ALERT (error-severity row +
  operator hint) when a node is on battery AND discharging AND at/below the 15%
  low threshold — the early warning the ms-mac crash lacked. Pure
  `power_doctor_checks` helper (the generic "telemetry field -> DoctorCheck row
  + alert" plumbing bd-f8d5e1 reuses).

## Diff summary

- Code commits: slice 1 `bd-ecd49b: power/battery status in node telemetry` and
  slice 2 `bd-ecd49b: caco doctor power-down row + alert`. Final landed squash
  SHAs from the reintegration receipts.
- Summary artefact commit: intentionally omitted (no self-reference).
- Files: `crates/caco-daemon/src/replication.rs` (slice 1),
  `crates/caco-cli/src/lib.rs` (slice 2), `SPEC.md` §12.6, `AGENTS.md`.
- Tests: `cargo test -p caco-daemon --lib bd_ecd49b` (4 pass: predicate, parts
  builder, pmset parser, linux time-remaining) + `cargo test -p caco-cli --lib
  power_doctor_checks` (1 pass). `cargo check --workspace --tests` clean.
- Behavioural delta: per-node power is visible fleet-wide and `caco doctor`
  alerts before a node dies on battery.

## Operator-takeaway

`caco doctor` now flags "ms-mac: on battery 8%, discharging, ~5min — imminent
power loss" before the crash, on any node (power propagates cross-node via the
snapshot). Threshold is 15%; AC/charging/no-battery nodes show a clean ok row.
The plumbing is generic so bd-f8d5e1 (location telemetry) drops a second
predicate into the same NodeTelemetry-field + doctor-row pattern.
