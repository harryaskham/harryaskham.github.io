# Session summary — Terraform-free AKS validation shell

## Goal

Split quick AKS status and server-side dry-run validation from the heavier ACA/Terraform provisioning shell so operators and agents can run production-safe checks without first building the Terraform/Terranix dependency closure.

## Bead(s)

- `bd-940a2a` — Split AKS status/dry-run dev shell from Terraform dependency closure

## Before state

- Failing tests: none known for this bead.
- Relevant metrics: previous validation attempted `nix develop .#aca --command just aks-self-dry-run` and timed out after 900 seconds while building `terraform-1.9.8-go-modules` before reaching kubectl/helm.
- Context: the existing `aks-self-status` and `aks-self-dry-run` recipes were already production-safe, but the documented hermetic shell pulled in provisioning dependencies that were unnecessary for status/dry-run loops.

## After state

- Failing tests: none observed.
- Relevant metrics: `just --dry-run aks-self-status-lite`, `just --dry-run aks-self-dry-run-lite`, `just --dry-run aks-lite-shell 'kubectl version --client'`, `nix eval --raw .#devShells.x86_64-linux.aks-lite.name`, `nix flake check --no-build`, and `deploy/aks/validate-operator-surfaces.sh` all passed.
- Context: the repo root now exposes `nix develop .#aks-lite` with kubectl, helm, caco, and python+PyYAML while intentionally excluding Terraform; just wrappers run the existing AKS status/dry-run recipes through that shell.

## Diff summary

- Commits: `9fab6f512`
- Files touched: `flake.nix`, `justfile`, `deploy/aks/README.md`, `deploy/aks/PRODUCTION-ROLLOUT.md`, `README.md`, `AGENTS.md`
- Tests: +0 Rust tests / +3 just wrapper recipes / +1 devShell.
- Behavioural delta: AKS quick validation has a first-party Terraform-free shell and documented `*-lite` recipes; full ACA/Terraform shell remains available for provisioning and nodepool recovery.

## Operator-takeaway

Use `just aks-self-status-lite` and `just aks-self-dry-run-lite` for fast production AKS inspection/dry-runs; reserve `nix develop .#aca` for flows that actually need Terraform or Azure provisioning tools.
