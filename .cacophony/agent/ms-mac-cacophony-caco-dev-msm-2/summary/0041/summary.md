# Session summary — router profile composition fix

## Goal

Fix the recurring `a.skh.am` router persistent-agent startup/reconcile failures that appeared after ms-mac daemon restarts, without mutating the hosted project directly. The goal was to make the checked-in Cacophony profile/config contract safe so the router can materialize again through normal daemon reconciliation.

## Bead(s)

- `bd-37a2ff` — a.skh.am router persistent agent failed during ms-mac restart startup

## Before state

- Failing evidence: repeated daemon log errors reported `Persistent agent router failed during startup` and `Persistent agent router failed during periodic reconcile` for project `a.skh.am`.
- Root-cause evidence: `caco log exceptions --since 8h --json` showed the actionable detail: `profile resolution failed: daemon error: composite profile composition failed: profile validation error: composite profile conflict on 'reintegration.mode': profile 'endless' sets 'direct' but profile 'router' sets 'none'`.
- Context: `caco agent list --project a.skh.am --json` returned no active agents, matching the reported routing degradation risk.

## After state

- Failing tests: none in the targeted validation lane.
- Validation passed:
  - `cargo test -p caco-profile router_profile_composes_with_endless_without_reintegration_conflict_bd_37a2ff`
  - `caco config validate --project-config-dir "$PWD/.cacophony" --json`
  - `git diff --check`
- Context: the router profile is still explicitly non-implementing, but its YAML lifecycle metadata is now direct-compatible with the shared `endless` persistent mixin, so `endless + router` composition no longer fails before launch.

## Diff summary

- Commits: local pre-reintegration commit on this branch; final squash SHA is assigned by direct reintegration
- Files touched: `.cacophony/profiles/router.md`, `crates/caco-profile/src/lib.rs`, `SPEC.md`, `README.md`, `AGENTS.md`, `.cacophony/agent/ms-mac-cacophony-caco-dev-msm-2/summary/pending/summary.md`
- Tests: added one focused `caco-profile` regression asserting the checked-in `endless` and `router` profiles compose successfully, retain direct reintegration metadata, retain cluster-controller scope, and preserve the prompt prohibition on router code landing.
- Behavioural delta: daemon persistent reconciliation should no longer fail the `router` declaration with a reintegration-mode profile conflict after this profile source lands and the live daemon/profile materialization catches up. Operators still need normal rollout/reconcile for already-running or failed persistent profile artifacts.
- Follow-up filed: draft `bd-11fefa` to teach config validation to compose persistent declaration profile stacks so this class of conflict is caught before runtime startup.

## Operator-takeaway

The router was not failing because `a.skh.am` itself was broken; it was inheriting Cacophony's shared `endless` lifecycle and then colliding with the router profile's old `reintegration.mode: none`. The profile is now composition-safe while still telling the router not to implement or land code.
