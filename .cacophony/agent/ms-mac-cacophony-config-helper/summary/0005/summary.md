# Session summary — cacophony-dev pr_auto_merge flip (O2-BACKEND)

## Goal

Land the cacophony-dev pr_auto_merge rollout as config: flip the cacophony Rust
dev-worker agents to land via DirectMerge-over-PR, while every other agent
(releasers, controllers, config-helper, and the ~12 other non-dev mode:direct
agents) keeps direct local-merge landing — and tag-push release agents keep
atomic semver-tag push. Authored under multi-agent review (aur-5/aur-2 +
ms-dev-2-ctrl); reintegrate is HELD pending their green-light + Harry's go-ahead.

## Bead(s)

- bd-259349 (per-profile reintegration backend override mechanism) — LANDED, enables this
- bd-560cad (allowed_modes last-wins layering fix) — LANDED, keeps allowed_modes untouched
- bd-1d514b (DirectMerge-over-PR auto-merge path) — the mechanism dev workers use

## Before state

- Failing tests: none (config-only)
- cacophony.integration: default_intent direct, backend local_merge (all agents land local_merge)
- An earlier OPTION-1 draft (project-backend flip + per-profile overrides) was authored then
  WITHDRAWN: an audit found it would wrongly route ~15 non-dev mode:direct agents through PR
  (controllers via controller.md/project-controller.md, project-health, caco-vm, caco-profile,
  changelog-manager, log-monitor, technical-writer, all 5 releasers, config-helper) — needing
  ~15 fragile overrides.
- `caco config validate`: ok

## After state

- Failing tests: none
- O2-BACKEND: project default backend STAYS local_merge; ONLY the 4 Rust dev-worker values
  (caco-dev, caco-dev-codex, caco-tui, caco-web) compose a new cacophony-pr-backend mixin
  (reintegration.backend: pull_request, bd-259349 per-profile override, last-wins). With
  default_intent: direct they land via DirectMerge-over-PR. project pr_base/reintegrate_target
  = origin/main (no-op land default for local_merge agents). ZERO non-dev profile changes.
- compose --dry-run: cacophony-pr-backend resolves reintegration.backend=pull_request (bd-733f1e);
  it is the only profile that sets backend, so non-dev agents inherit local_merge.
- Surface specialists (caco-android/ios/macos) DEFERRED to a 2nd pass (their values are shared
  dev+releaser; a shared-value mixin would wrongly flip the releaser).
- `caco config validate`: ok; docs/profiles.html regenerated (new mixin row)

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt
- Files: `.cacophony/projects.yaml` (+pr_base/reintegrate_target + O2 comment; backend stays
  local_merge), `.cacophony/agents/cacophony_persistent.yaml` (compose cacophony-pr-backend on
  the 4 dev values), `.cacophony/profiles/cacophony-pr-backend.md` (new mixin), `docs/profiles.html`
  (regen). NET: 3 config files + docs; ZERO non-dev profile changes.
- Tests: +0 / -0
- Behavioural delta: cacophony Rust dev workers land via DirectMerge-over-PR; everything else
  unchanged (local_merge); release tag-push unaffected.

## Operator-takeaway

The first cacophony pr_auto_merge slice ships as a tightly-scoped backend mixin on the 4 Rust
dev-worker values, NOT a project-wide flip — so it touches zero non-dev agents and cannot break
release tag-push or controller landing. Surface specialists (android/ios/macos) are a deliberate
2nd pass once their shared dev/releaser values are split. A temporary Rust-devs-PR /
surface-devs-direct incremental-rollout state is expected and fine.
