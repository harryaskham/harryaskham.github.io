# Session summary — caco-aks persistent profile

## Goal

Capture the full AKS rollout workflow while it was fresh by adding a dedicated persistent `caco-aks` profile that can run on `ms-mac`, stay focused on AKS-only work, coordinate around current blockers, and land fixes via PR-backed reintegration with direct fallback.

## Bead(s)

- `bd-0f3d49` — Encode caco-aks persistent profile (`<repo>/.cacophony/profiles/caco-aks.md`) — AKS cluster setup loop
- Current blocker pointer: `bd-535c46` — Repair Azure CLI / AKS node recovery path on ms-mac
- Current rollout pointer: `bd-decf57` — Roll out self-contained AKS multi-role topology to production

## Before state

- Failing tests: none known for this scope.
- Relevant metrics: no `caco-aks` canonical profile existed; `docs/profiles.html` had no shipped-profile row; no ms-mac persistent declaration pointed at an AKS rollout-owner profile.
- Context: the AKS implementation stream had landed Terranix, self-contained config, multi-role Helm, in-cluster PKI/bootstrap, private web/SSH/TUI access, operator validation surfaces, production rollout notes, and the non-CA `caco supervisor` Helm fix. The remaining live rollout state was split across beads and handoff messages.

## After state

- Failing tests: none in scoped validation.
- Relevant metrics: `caco config validate --config .cacophony/config.yaml` returned `config valid` (with the expected non-blocking pre-sync warning that the new profile is not yet in the daemon's configured profile registry); `cargo test -p caco-profile caco_aks_profile_loads_as_persistent_pr_backed_loop` passed; `cargo test -p caco-profile shipped_profiles_html_lists_every_canonical_profile` passed.
- Context: `.cacophony/profiles/caco-aks.md` now exists, is parse-tested, is documented in `docs/profiles.html`, and is wired into `.cacophony/agents/cacophony_persistent.yaml` for `ms-mac` with no generic autoclaim.

## Diff summary

- Commits: `1bf685d84` (profile/config/docs/test change; this summary is committed as sibling session-recording commits)
- Files touched: `.cacophony/profiles/caco-aks.md`, `.cacophony/agents/cacophony_persistent.yaml`, `docs/profiles.html`, `crates/caco-profile/tests/profile.rs`
- Tests: +1 profile integration test for `caco-aks`; shipped-profile docs drift test still passes.
- Behavioural delta: the repo now has a first-party persistent profile and declaration for an AKS rollout owner that knows the current bd-535c46/bd-decf57 handoff, the AKS validation/deploy order, hermetic shell expectations, PR-backed reintegration with direct fallback, and the no-autoclaim endless loop policy.

## Operator-takeaway

Future AKS work can be resumed by a dedicated `caco-aks` persistent agent on `ms-mac` instead of relying on scattered session memory: it should hydrate the rollout notes and current beads, avoid unrelated queue work, coordinate with node-recovery owners, and keep production rollout fixes landing.
