# Session summary — ACA Attic Nix cache retired + cacheless workaround docs

## Goal

Harry retired the ACA-hosted Attic Nix binary cache (ms-mac DM, 2026-06-22;
ACA settings changes made it untenable), reversing the earlier bd-7dcbdc note
that called it operator-managed and still active. caco-ctrl and the ms-dev-2
agents then empirically corrected the interim cacheless flag: the fleet-robust
form is `nix --option substituters 'https://cache.nixos.org'` (keep the public
nixpkgs cache, drop only the dead ACA/redhill substituter), NOT bare `''`, which
source-storms cold-store builders and can hang the daemon test/reint gate on the
dead substituter. This session updates the canonical docs to the corrected,
robust guidance.

## Bead(s)

- `bd-2a8743` — Docs: ACA Attic Nix cache is now retired — correct
  AGENTS.md/README.md + document the cacheless interim workaround.

## Before state

- Failing tests: none (docs-only).
- AGENTS.md (already corrected on main by a concurrent agent) and README.md both
  documented the bare `nix --option substituters ''` form, which the fleet later
  determined is warm-store-only and unsafe on cold-store builders.

## After state

- Failing tests: none.
- README.md documents the fleet-robust
  `nix --option substituters 'https://cache.nixos.org'` form (keep public cache,
  drop only dead ACA/redhill), notes bare `''` is warm-store-only / cold-store
  source-storm + gate-hang risk, points at the host-side durable fix
  (`/etc/nix/nix.custom.conf`), and notes the repo carries no ACA/redhill
  substituter references plus the planned local atticd. AGENTS.md was already
  corrected to the same robust form on main by a concurrent agent, so this
  change leaves AGENTS.md as main's version (the redundant edit was dropped in
  rebase).

## Diff summary

- Code/content commit: pending final squash SHA from the reintegration receipt.
- Files touched: README.md (1 paragraph). AGENTS.md not touched (a concurrent
  agent already landed the equivalent robust-form correction on main). Docs-only
  Markdown; no generated HTML siblings, so no docs-check/cargo validation needed.
- Tests: +0 / -0. No code behaviour change.
- Behavioural delta: documentation guidance only.

## Operator-takeaway

The ACA-hosted Attic Nix cache is retired (2026-06-22). Until local atticd runs
on ms-dev / ms-dev-2 / ms-dev-3, run nix with
`nix --option substituters 'https://cache.nixos.org'` (keep the public cache,
drop only the dead ACA/redhill substituter) — the robust form for cold-store
builders. Bare `''` only works on warm stores and source-storms / gate-hangs
cold stores. The durable fix is host-side removal of the dead ACA line from
`/etc/nix/nix.custom.conf`.
