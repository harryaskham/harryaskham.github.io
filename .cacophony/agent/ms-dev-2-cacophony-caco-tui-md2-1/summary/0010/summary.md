# Session summary — AGENTS.md bd-2bbb1c inherit-host doc-sync (bd-716f8e/bd-109ab3)

## Goal

Sync the AGENTS.md bd-2bbb1c prose to the landed behavior after bd-716f8e
flipped the queue nix-develop substituters default from pin-cache.nixos.org to
inherit-host, so the canonical agent doc describes the real default + the
opt-in pin escape rather than the old pinned default.

## Bead(s)

- `bd-716f8e` — flip queue nix-develop substituters default to inherit-host (aur-3, landed 0eaeff67b).
- `bd-109ab3` — reconcile Nix-cache docs (AGENTS.md slice; technical-writer owns README/deploy).

## Before state

- AGENTS.md L191/L233 said queued cargo "pins --option substituters
  https://cache.nixos.org (the public cache only) by default" — stale the
  moment bd-716f8e (0eaeff67b) landed the inherit-host default.

## After state

- L191/L233 now say queued cargo INHERITS the host substituters by default
  (uses the cleaned host's cache.nixos.org + new ms-dev/ms-dev-2 collective
  caches); CACO_QUEUED_NIX_SUBSTITUTERS=<list> is the opt-in explicit pin
  (injects --option substituters + extra-substituters ''), framed as the
  universal escape for a dead substituter from any of the 3 layers (dirty host
  config / Category B nixos-rebuild, stale flake eval-cache, stale in-memory
  daemon / Category A restart).

## Diff summary

- Files touched: AGENTS.md (L191 + L233 bd-2bbb1c sentences).
- Code/content commit: pending final landed SHA from the reintegration receipt.
- Behavioural delta: docs-only; documents the landed inherit-host default,
  landed AFTER 0eaeff67b per the after-the-code timing.

## Operator-takeaway

Landed after the code (0eaeff67b) so the doc matches the deployed default, not
the proposal. The opt-in pin is now the cross-layer escape: inherit-host uses
whatever the host resolves, so a node with a dead substituter in host config /
eval-cache / in-memory daemon must set CACO_QUEUED_NIX_SUBSTITUTERS to override.
