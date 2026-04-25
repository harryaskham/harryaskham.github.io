# Session summary — microVM worker preflight

## Goal

Add a narrow, provider-neutral preflight surface that tells operators and future schedulers whether a worker node is ready to admit microVM-wrapped transient agents. This intentionally avoids overlapping the active scheduler contract and Cloud Hypervisor runner beads.

## Bead(s)

- `bd-c61ad0` — [microvm] Add worker-node microVM capability probe and cache preflight
- Related design: `bd-0a9042` — Explore microVM-wrapped transient agent jobs on worker nodes

## Before state

- Failing tests: none known for this bead.
- Relevant metrics: there was no `caco microvm` command family or local readiness report for `/dev/kvm`, kernel modules, tun/tap, vsock, cgroup v2, nftables, hypervisor binaries, or microVM image/cache directories.
- Context: sibling beads are actively handling the provider-neutral scheduler contract and Cloud Hypervisor prototype, so this work needed to stay read-only and detection-focused.

## After state

- Failing tests: none observed.
- Relevant metrics: `cargo test -p caco-cli microvm_preflight -- --nocapture` passed; `cargo check -p caco-cli --tests` passed; `cargo test-small` passed; `git diff --check` passed.
- Context: `caco microvm preflight [--cache-dir <path>]` now reports `ready`, per-check statuses, cache root, and remediation hints in both text and JSON. A live run on this host reports `ready=false` because vsock, nftables, hypervisor binary, and cache directories are missing.

## Diff summary

- Commits: `d8f0ea7f9`.
- Files touched: `crates/caco-cli/src/lib.rs`, `SPEC.md`, `README.md`, `AGENTS.md`, `docs/investigations/bd-0a9042-microvm-agent-jobs.md`, `.cacophony/agent/ms-dev-cacophony-caco-dev-msd-1/summary/0023/summary.md`.
- Tests: added CLI unit coverage for command registration, all-required-capabilities-ready, and missing-capabilities-blocking.
- Behavioural delta: operators and future placement code now have a first-party readiness surface for microVM worker nodes, without changing default host-agent launches.

## Operator-takeaway

The new preflight is a hard gate signal for future microVM routing: do not send isolated transient agents to a node until `caco microvm preflight` says it is ready or the missing capabilities are intentionally handled by a backend-specific follow-up.
