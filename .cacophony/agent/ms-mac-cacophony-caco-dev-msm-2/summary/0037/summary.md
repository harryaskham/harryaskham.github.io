# Session summary — AKS self-contained beads branch isolation

## Goal

Prevent the self-contained AKS validation topology from acting as an independent writable beads primary against the production cluster's canonical `beads` branch.

## Bead(s)

- `bd-9069b5` — [AKS] self-contained beads primary can force-push stale board over production beads branch

## Before state

- Failing tests: none specific; the bug was observed during AKS verification when in-cluster sync exported stale AKS board state to the shared production `beads` branch.
- Relevant metrics: AKS topology rendered `beads.primary: caco-aks-master-0` and inherited the project beads branch from shared project config, which is `beads`.
- Context: self-contained AKS is useful for validation, but a cluster-local primary with cluster-local state must not share the production authoritative branch unless it joins the real primary/fencing topology.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: AKS topology now defaults `projects.cacophony.beads.branch` to `aks-beads`; renderer validation rejects an explicit `CACO_AKS_BEADS_BRANCH=beads` in self-contained mode.
- Context: documentation and SPEC now state that validation clusters must proxy to production, join as fenced/warm-standby candidates, or use a distinct branch/remote.

## Diff summary

- Commits: `e6f9d8656`
- Files touched: `deploy/aks/config/topology.yaml`, `deploy/aks/render-config.sh`, `deploy/aks/validate-self-contained-config.sh`, `deploy/aks/validate.sh`, `deploy/aks/README.md`, `deploy/aks/PRODUCTION-ROLLOUT.md`, `SPEC.md`, `crates/caco/tests/deploy_assets.rs`
- Tests: `cargo fmt --all -- --check`; `cargo test -p caco --test deploy_assets aks_self_contained_topology_uses_isolated_beads_branch`; `./deploy/aks/validate-self-contained-config.sh`; `CACO_BIN=\"cargo run -q -p caco --\" ./deploy/aks/validate-self-contained-config.sh`; `./deploy/aks/validate.sh`; `git diff --check`
- Behavioural delta: self-contained AKS renders to an isolated beads branch by default and fails closed if configured to use the production `beads` branch.

## Operator-takeaway

AKS validation can still run its own in-cluster primary, but it is no longer allowed to push stale validation board state over production beads; using the production branch now requires the explicit production primary/fencing model instead.
