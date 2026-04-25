# Session summary — Managed-agent isolation backend contract

## Goal

Specify the first normative config and product contract for optional managed-agent isolation backends so microVM work can proceed without changing bead ownership semantics or overlapping the separate scheduler, capability-probe, and Cloud Hypervisor prototype beads.

## Bead(s)

- `bd-3989e0` — [microvm] Specify managed-agent isolation backend contract
- Parent investigation: `bd-0a9042` — Explore microVM-wrapped transient agent jobs on worker nodes

## Before state

- Failing tests: none known for this scope.
- Relevant metrics: `docs/investigations/bd-0a9042-microvm-agent-jobs.md` contained the design note and follow-up split, but `SPEC.md` and the config schema did not yet define `agent_defaults.isolation`, backend names, disk resource limits, mount/network policy, or fallback semantics.
- Context: other agents were actively working adjacent slices (`bd-f285db` provider-neutral scheduler model, `bd-c61ad0` capability preflight, `bd-70591a` Cloud Hypervisor runner), so this session deliberately stayed on the config/SPEC contract only.

## After state

- Failing tests: none observed in targeted validation.
- Relevant metrics: `cargo check --workspace --tests` passed; `cargo test -p caco-config isolation --lib` passed; `cargo test -p caco-config resource_limits --lib` passed; `cargo run -q -p caco -- config validate --config .cacophony/config.yaml` passed with only pre-existing warnings.
- Context: `agent_defaults.isolation` now has typed config fields and validation for backend enum, unavailable-backend policy, mount modes, network mode, and resource disk size.

## Diff summary

- Commits: current HEAD for this summary chunk (`bd-3989e0: specify agent isolation backend contract`)
- Files touched: `SPEC.md`, `crates/caco-config/src/model.rs`, `crates/caco-config/src/validate.rs`, `crates/caco-daemon/src/agent/tests.rs`, `crates/caco-daemon/src/lib.rs`, `crates/caco-daemon/src/spawn_routing.rs`, `.cacophony/agent/ms-dev-cacophony-caco-dev-msd-5/summary/0025/summary.md`
- Tests: added config validation coverage for isolation config and disk resource limits; updated daemon fixtures for the expanded resource/default structs.
- Behavioural delta: config now accepts and validates isolation backend requests (`host`, `cloud-hypervisor`, `firecracker`, `kata`, `oci`) and rejects invalid backend/fallback/mount/network values before runtime. `resources.disk` is available for isolation backend admission without changing current host-backed agent behaviour.

## Operator-takeaway

This lands the contract layer, not the runner. MicroVM and dynamic-compute implementation work can now rely on a shared vocabulary for requested versus actual backend, fail-vs-fallback behaviour, resource enforcement, mount/network boundaries, and operator-visible backend state.
