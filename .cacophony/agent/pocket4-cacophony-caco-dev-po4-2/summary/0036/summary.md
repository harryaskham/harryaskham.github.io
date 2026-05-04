# Session summary — persistent specialist guard disables worker-pool slash commands for caco-aks

## Goal

Continue the profile-audit cleanup after the `bd-1cd45f` baseline split by addressing the next directly related conflict: `caco-aks` was still inheriting `pi-self-ops` helper text through shared Pi runtime imports, which made `/bead-claim` and other worker-pool lifecycle commands appear available even though the AKS role is an endless, scoped, no-autoclaim specialist.

## Bead(s)

- `bd-0ee3c9` — [profile-audit] pi-self-ops bead-claim command conflicts with caco-aks lifecycle
- related in-flight context: `bd-1cd45f` — [profile-audit] endless generic claim instruction conflicts with caco-aks no-autoclaim

## Before state

- `caco-aks` had already been moved off the generic `persistent.yaml` endless mixin onto a new `persistent-specialist.yaml` baseline.
- That baseline still imported `base.yaml`, which pulls in `pi-common.yaml`, which in turn composes `pi-self-ops`.
- As a result, the reified prompt for `caco-aks` could still expose worker-pool slash-command language such as `/bead-claim`, `/self-idle`, and `/pool-register`, despite the `caco-aks` role profile explicitly forbidding generic autoclaim and owning its own scoped lifecycle.
- A targeted profile test in `crates/caco-profile/tests/profile.rs` also still asserted on a stale historical `bd-535c46` string that no longer exists in the raw `caco-aks.md` profile text.

## After state

- Added a new prompt guard profile:
  - `.cacophony/profiles/persistent-specialist.md`
- Updated `.cacophony/agents/persistent-specialist.yaml` to compose:
  - `persistent-specialist`
- Updated the `caco-aks` declaration in `.cacophony/agents/cacophony_persistent.yaml` to compose profiles in this order:
  - `persistent-specialist`
  - `caco-aks`
- The new guard explicitly disables generic worker-pool helper behavior for specialist persistent roles unless the role profile explicitly opts into it, including:
  - `/bead-claim`
  - `/self-idle`
  - `/pool-register`
  - no-ID `caco bd claim` queue-drain loops
- Refreshed the targeted `caco-aks` profile test to assert current semantic strings instead of a stale historical bead ID.

## Diff summary

- Files touched:
  - `.cacophony/profiles/persistent-specialist.md` (new)
  - `.cacophony/agents/persistent-specialist.yaml`
  - `.cacophony/agents/cacophony_persistent.yaml`
  - `crates/caco-profile/tests/profile.rs`
- Tests / validation:
  - `cargo test -p caco-profile persistent_specialist_profile_disables_generic_pool_helpers -- --nocapture`
  - `cargo test -p caco-profile caco_aks_profile_loads_as_persistent_direct_loop_with_pr_modes_available -- --nocapture`
  - `cargo build -p caco-config`
  - `cargo run -p caco -- --config .cacophony/config.yaml status`
- Behavioural delta:
  - specialist persistent roles now have an explicit prompt-side guard against worker-pool slash-command leakage
  - `caco-aks` keeps its scoped AKS workflow instead of appearing eligible for `/bead-claim`/pool rotation helpers
  - the targeted profile test now tracks current caco-aks semantics instead of a stale bead reference

## Operator-takeaway

`bd-1cd45f` fixed the generic endless next-task leakage; `bd-0ee3c9` fixes the remaining worker-pool slash-command leakage. Together they establish a reusable “persistent specialist” pattern: specialist endless roles can still use shared Pi/runtime helpers, but they no longer inherit the parts of the pool-worker contract that would push them toward claim-complete-claim-next behavior.
