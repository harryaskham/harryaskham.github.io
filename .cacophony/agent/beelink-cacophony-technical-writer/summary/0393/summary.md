# Technical-writer review summary

## Goal

Correct an over-removal in the cache-doc reconciliation: restore the ms-mac
eval-cache caveat (gated on an ms-mac-local eval-cache-refresh confirmation) that
015ea6299 dropped, while keeping the redhill-removed + new-caches-live prose.

## Bead(s)

- `bd-109ab3` — technical-writer/ctrl cache comment+doc cleanup (my README + deploy slice).
- `bd-c63005` — technical-writer documentation maintenance.

## Before state

- 015ea6299 (mine) removed the "avoid ms-mac queued cargo / keep ms-mac override" caveat across the cache docs, treating Harry's "redhill removed from all running systems" as covering ms-mac too. cluster-ctrl + md2-1 flagged that the ms-mac nix EVAL-CACHE is a separate layer: host-substituters-clean does NOT confirm the eval-cache is refreshed, so dropping the caveat makes the docs say "safe when it isn't."

## After state

- README.md (L317/L845) + deploy/aca/README.md restore the ms-mac caveat: ms-mac's eval-cache is a separate layer that may still pin dead redhill until refreshed, so keep the ms-mac `cache.nixos.org` override on one-off macOS nix commands and avoid queued cargo on ms-mac until an operator / ms-mac-ctrl confirms the eval-cache is refreshed. Notes the coupled config-side workarounds (automation.yaml override — already removed at 6047e500b — and caco-macos.md instruction) share the same ms-mac-eval-cache gate, routed to ms-mac-ctrl / caco-macos specialist. Redhill-removed + new ms-dev/ms-dev-2-caches-live prose kept.
- AGENTS.md slice (L191/L233) is owned by md2-1 (restoring the same caveat on top of 015ea6299, ctrl-reviewed); caco-macos.md stays with ms-mac-ctrl. Validate-pages passed.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `README.md`, `deploy/aca/README.md`.
- Behavioural delta: documentation only.

## Operator-takeaway

The cache docs no longer claim ms-mac is safe for queued cargo before its
eval-cache refresh is confirmed — the redhill-removed/new-caches reality and the
ms-mac eval-cache caveat now coexist correctly.
