# Session summary — AGENTS.md ms-mac eval-cache caveat restore (bd-109ab3 slice)

## Goal

Complete the AGENTS.md slice of the redhill-removal doc cleanup (bd-109ab3) by
fixing an over-removal: AGENTS.md had dropped the avoid-queued-cargo-on-ms-mac
caveat as "retired" while README/deploy kept it, leaving the canonical agent
doc telling agents ms-mac queued cargo is safe when it is not.

## Bead(s)

- `bd-109ab3` — reconcile Nix-cache docs to the live ms-dev/ms-dev-2 collective
  caches (AGENTS.md slice; technical-writer/beelink owns README/caco-macos/deploy).

## Before state

- AGENTS.md L191/L233 said "the per-node cache.nixos.org overrides and the
  avoid-queued-cargo-on-ms-mac workaround are now retired (reverts owned by
  bd-b4cc45)" — but README/deploy (fd91c80df) had restored that caveat because
  the ms-mac stale flake eval-cache is not yet confirmed refreshed, and aur-2
  reverted a premature override removal (37f315fcb). Inconsistent + unsafe.
- Failing tests: none (docs-only).

## After state

- AGENTS.md L191/L233 now say the ms-mac cache.nixos.org override + the
  avoid-queued-cargo-on-ms-mac caveat are KEPT until an operator/ms-mac-ctrl
  confirms the eval-cache is refreshed (host-substituters-clean does not by
  itself confirm refresh); bd-b4cc45's override-revert is gated on that.
  Mirrors beelink's corrected README/deploy wording.
- Failing tests: none (docs-only).

## Diff summary

- Files touched: AGENTS.md (L191 + L233, two sentences).
- Code/content commit: pending final landed SHA from the reintegration receipt.
- Behavioural delta: docs-only; re-aligns the canonical agent doc with
  README/deploy so an agent does not run ms-mac queued cargo into the redhill
  eval-cache hang.

## Operator-takeaway

A full-rewrite of these notes was redundant (main already had the redhill/new-
caches update via 015ea6299), but the rebase-conflict investigation surfaced a
real safe-when-it-isn't bug: fd91c80df's caveat-restore correction reached
README/deploy but missed AGENTS.md. The ms-mac eval-cache is still the one
node-specific caveat that must hold until ms-mac-ctrl confirms the refresh.
