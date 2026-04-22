# Session summary 0018 — bd-5c2a98 slice 1: peer-version observability

## Goal

Make cross-node version drift observable in `caco doctor` without
extra probe round-trips. Operators noticed helsinki on v1.2.489 while
ms-dev/winmini on v1.2.460 (29 versions behind) but had no first-party
surface to detect it — required manual `caco msg speak` correlation.

## Bead(s)

- `bd-5c2a98` slice 1 — peer-version observability via existing
  config-hash probe + doctor area.

## Before state

- `PeerReachability` carried `config_hash`, `disk_hash`, `restart_pending`
  but not the daemon binary version.
- `caco doctor` had no version-drift signal; operators correlated
  manually from restart-broadcast bodies.
- 29-version gaps could persist silently.

## After state

- `ConfigHashInfo` (the body of `GET /api/v1/config/hash`) gains
  optional `version` field populated from `env!(CARGO_PKG_VERSION)`.
- `ConfigProbeSuccess` and `PeerReachability` gain matching
  `version` / `peer_version` fields, populated by the existing
  config-hash probe path. Sticky-on-None semantics: a transient
  probe that omits the field doesn't flap the cached version.
- Self-entry reports the local binary version so single-node installs
  show a non-trivial version row immediately.
- `caco doctor` `node_health` rows render `version=X.Y.Z` inline
  when known.
- New "Version Drift" doctor section emitted only when ≥2 distinct
  versions are observed across reporting peers (peers with
  unknown version don't trigger false drift). Lists each
  `node version=...` so operators can see which to upgrade.

## Diff summary

- Commit: `3637eef5`.
- Files (3): `crates/caco-daemon/src/lib.rs` (ConfigHashInfo,
  doctor JSON, peer init), `crates/caco-daemon/src/replication.rs`
  (probe parse + write + self-entry version), `crates/caco-cli/src/lib.rs`
  (doctor renderer + version-drift area).
- Tests: none added (smoke-validate by running `caco doctor` post-merge
  against fleet).
- `cargo build -p caco-daemon -p caco-cli`: clean.
- `cargo clippy -p caco-daemon -p caco-cli`: clean (only pre-existing
  unrelated caco-cli warnings).

## Out of scope (deferred)

- **Active alerting** (file a bead on drift threshold) — needs
  operator policy decision on threshold value + cooldown to avoid
  spam during legitimate upgrade rollouts.
- **`caco fleet versions` standalone CLI** — the doctor area covers
  the immediate operator need; standalone subcommand can come later
  if `caco doctor` proves too noisy as the discovery surface.
- **update-helper auto-converging lagging nodes** — sister bead
  bd-aa882f (separate workstream).

## Operator-takeaway

`caco doctor` now surfaces a "Version Drift (N distinct versions...)"
section whenever your fleet is running mixed versions. The section
lists each `node version=X.Y.Z` so you can see at a glance which
nodes are lagging. Drift on a few versions is normal during a
rollout; persistent 20+ version gaps (like tonight's helsinki vs
ms-dev) are a wire-protocol risk that the area now makes obvious.
