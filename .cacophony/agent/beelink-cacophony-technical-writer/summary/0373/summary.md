# Technical-writer summary — cacheless docs: ms-mac eval-cache caveat

## Goal

Refine the cacheless-Nix docs with the ms-mac caveat both controllers flagged.
The prior scoped flip (a2c5f1433) listed ms-mac among the cleaned nodes (plain nix,
no override). Correction: ms-mac's HOST config is clean (Determinate, nix show-config
clean), BUT its QUEUED nix-develop cargo path still hits a stale collective eval-cache
pinning the dead redhill (504, confirmed 2x) — because ms-mac got only the Determinate
revert, not a full nixos-rebuild switch like ms-dev/ms-dev-2. An ms-mac agent reading
"ms-mac cleaned → plain nix" would drop the override and hit the queued-cargo hang.

## Bead(s)

- No implementation bead — operator/controller-directed documentation accuracy
  refinement (technical-writer maintenance). 4th/final revision of the cacheless
  guidance; the ms-mac eval-cache fix itself is operator/ms-mac-ctrl lane.

## Before state

- AGENTS.md (L191 + Dev Workflow pointer), README, deploy/aca/README all listed
  ms-dev + ms-dev-2 + ms-mac as cleaned (plain nix, no override) — incorrect for
  ms-mac's queued cargo path.

## After state

- All four surfaces now split per-node: ms-dev + ms-dev-2 fully cleaned by
  nixos-rebuild (plain nix, no override); ms-mac host clean but queued nix-develop
  cargo KEEPS the cache.nixos.org override until the ms-mac eval-cache fix lands
  (operator/ms-mac-ctrl lane); ms-dev-3/beelink unconfirmed and may still carry
  redhill in host config. The override is documented as a stopgap wherever the dead
  redhill is still reachable (host config OR stale eval-cache).

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `AGENTS.md` (×2), `README.md`, `deploy/aca/README.md`. No `docs/`
  HTML siblings. No AUTOGEN/previous-summary churn in AGENTS.md.
- Tests: n/a (docs-only).
- Behavioural delta: documentation only.

## Operator-takeaway

The cacheless docs now accurately reflect the per-node state so ms-mac hands don't
drop the override and hit the queued-cargo eval-cache hang. Cacheless arc: bare ''
→ cache.nixos.org → resolved → scoped → ms-mac eval-cache caveat (the genuinely
final accurate state). Revert ms-mac's caveat once its eval-cache fix lands.
