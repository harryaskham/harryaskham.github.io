# Session summary — guarded AKS deploy surface

## Goal

Make AKS a more straightforward operator surface by stopping the repeated 30-minute ACR rebuild loop, adding an explicit AKS Azure/ACR shell alias, and documenting a guarded deploy path that distinguishes image freshness from in-pod repo and beads freshness.

## Bead(s)

- `bd-2d1ffe` — Restore AKS cluster health and ergonomic local access

## Before state

- Failing tests: none in AKS health, but the operator loop was inefficient: `just deploy-remotes` rebuilt the image for every moving `origin/main`, even when main changed docs, Android, macOS, web-only assets, or version metadata.
- Relevant metrics: AKS was healthy with eight Ready nodes, all role pods Running, revision 47 on image `2ff1b711f710`, private `@cluster` access working, and AKS beads fresh on `aks-beads`.
- Context: the heavy Azure/ACR shell was still named `.#aca`, making AKS rollout commands look like retired ACA work.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: `just --summary` includes `aks-deploy-main` and `aks-shell`; `nix eval .#devShells.<system>.aks.drvPath` succeeds; `./deploy/aks/validate.sh` reports 82 passed, 0 warnings, 0 failed.
- Context: AKS operators now have `nix develop .#aks` for the heavy Azure/ACR path and `just aks-deploy-main` for guarded current-main rollout. The guard skips ACR/Helm when no AKS-runtime-relevant files changed, refreshes the in-pod checkout when possible, and leaves `just deploy-remotes` as the explicit unconditional rebuild escape hatch.

## Diff summary

- Commits: `7b99c054f`
- Files touched: `justfile`, `flake.nix`, `README.md`, `AGENTS.md`, `deploy/aks/README.md`, `deploy/aks/PRODUCTION-ROLLOUT.md`, `.cacophony/profiles/caco-aks.md`
- Tests: `just --summary`, `nix eval .#devShells.<system>.aks.drvPath`, `./deploy/aks/validate.sh`
- Behavioural delta: adds a first-class AKS deploy shell alias and guarded deploy recipe that prevents unnecessary remote image builds for non-AKS-runtime drift.

## Operator-takeaway

AKS is operational, and the mainline operator path is now less wasteful: use `nix develop .#aks --command just aks-deploy-main` for normal image freshness and reserve `just deploy-remotes` for intentional unconditional rebuilds.
