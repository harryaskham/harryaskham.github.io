# Session summary — Firecracker and Kata microVM evaluation

## Goal

Evaluate Firecracker and Kata as future Cacophony managed-agent isolation backends after the Cloud Hypervisor prototype update, without changing default host-backed agent behaviour.

## Bead(s)

- `bd-35aa2c` — [microvm] Evaluate Firecracker and Kata backends after the Cloud Hypervisor prototype
- related: `bd-70591a` — [microvm] Prototype Cloud Hypervisor one-job managed-agent runner

## Before state

- Failing tests: none.
- Relevant metrics: no Linux KVM/Kata measurements were available from this macOS checkout; evaluation needed to define what must be measured on real Linux/Kubernetes workers.
- Context: the existing microVM investigation recommended Cloud Hypervisor first and reserved Firecracker/Kata, but did not yet capture the post-prototype decision shape or concrete follow-up beads.

## After state

- Failing tests: none in docs smoke validation.
- Relevant metrics: the new evaluation defines a backend-neutral measurement table covering cold/warm start, runtime readiness, daemon bridge, attach/log readiness, completion, teardown, crash checkpointing, idle overhead, and resource enforcement.
- Context: Firecracker is documented as the hardened local-worker lane that should wait for guest-private checkout/export; Kata is documented as the Kubernetes RuntimeClass/dynamic-worker lane rather than local-daemon hypervisor management.

## Diff summary

- Commits: `cea5a1784`
- Files touched: `docs/investigations/bd-35aa2c-firecracker-kata-evaluation.md`, `docs/investigations/bd-0a9042-microvm-agent-jobs.md`, `SPEC.md`
- Tests: docs link/content smoke via Python; `cargo fmt --all -- --check`.
- Behavioural delta: no runtime behaviour changed. The product contract now explicitly reserves Firecracker for hardened guest-private export and Kata for Kubernetes/containerd RuntimeClass-backed dynamic workers.
- Board follow-ups: filed drafts `bd-03f730` (Firecracker prototype), `bd-ff9c2d` (Kata RuntimeClass prototype), `bd-3dd5e6` (backend comparison harness), and reflection draft `bd-01d272` (`caco agent diff` should show untracked files).

## Operator-takeaway

Do not spend implementation cycles making Firecracker or Kata the next default backend yet: finish Cloud Hypervisor plus shared artifact/checkpoint hardening first, then use Firecracker for local hardening and Kata for AKS/dynamic worker RuntimeClass integration.
