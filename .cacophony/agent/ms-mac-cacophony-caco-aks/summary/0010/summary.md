# Session summary — narrow AKS rebuild guard

## Goal

Refine the newly added AKS guarded deploy path so it actually prevents unnecessary ACR rebuilds instead of still treating operator docs, profile notes, justfile changes, and other non-image drift as image rebuild triggers.

## Bead(s)

- `bd-2d1ffe` — Restore AKS cluster health and ergonomic local access

## Before state

- Failing tests: none in AKS health, but the first guarded deploy implementation still had too broad a file filter.
- Relevant metrics: AKS was healthy with eight Ready nodes, all role pods Running, and private `@cluster` access/beads sync working.
- Context: the guard needed to distinguish image inputs, Helm chart inputs, and in-pod repo freshness as separate axes.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: `just --summary` parses successfully, and `./deploy/aks/validate.sh` reports 82 passed, 0 warnings, 0 failed.
- Context: `aks-deploy-main` now limits image rebuild inputs to Dockerfile, Cargo metadata, runtime crates used by AKS, and the shared container prelude; Helm-only changes are applied with the existing image; non-image/non-Helm drift refreshes the in-pod checkout without ACR/Helm work.

## Diff summary

- Commits: `32a6ae244`
- Files touched: `justfile`, `README.md`, `AGENTS.md`, `deploy/aks/README.md`, `deploy/aks/PRODUCTION-ROLLOUT.md`
- Tests: `just --summary`, `./deploy/aks/validate.sh`, `git diff --check`
- Behavioural delta: the AKS deploy command now avoids remote image builds for docs, Android/macOS, profile notes, justfile-only edits, and other non-image drift while still handling Helm-only changes safely.

## Operator-takeaway

Use `nix develop .#aks --command just aks-deploy-main` as the normal AKS rollout/freshness command; it should now skip the expensive ACR build unless image inputs actually changed.
