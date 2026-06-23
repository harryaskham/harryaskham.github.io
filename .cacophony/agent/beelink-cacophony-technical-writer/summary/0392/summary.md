# Technical-writer review summary

## Goal

Reconcile the Nix-binary-cache docs to the operator-confirmed new state: redhill
removed from all running systems and the new self-hosted ms-dev/ms-dev-2 caches
live with all nodes switched (Harry's 2026-06-23 directive; doc half of bd-109ab3).

## Bead(s)

- `bd-109ab3` — technical-writer/ctrl comment+doc cleanup (doc half here; config-comment/profile half is ctrl/aur-3).
- `bd-c63005` — technical-writer documentation maintenance.
- Related: `bd-b4cc45` (config-side override revert, aur-3), `bd-2bbb1c`/`bd-888b1a` (queue/release cacheless substituter).

## Before state

- AGENTS.md (L191/L233), README.md (L317/L845), deploy/aca/README.md, deploy/aks/PRODUCTION-ROLLOUT.md said "currently NO Attic binary cache" + "a local atticd is PLANNED on ms-dev/ms-dev-2/ms-dev-3 (not yet running)" and prescribed the per-node `cache.nixos.org` override + "AVOID queued cargo on ms-mac" / "unconfirmed nodes may still carry redhill" workarounds — all now stale.

## After state

- All four docs now state: the new self-hosted `ms-dev`/`ms-dev-2` Nix binary caches are live and all fleet nodes have switched onto them; redhill is removed from all running nixos/nix-darwin/nixondroid systems; the repo carries no redhill/ACA substituter pin (only the unrelated `nixpkgs-terraform.cachix.org`); the per-node overrides and avoid-queued-cargo-on-ms-mac workaround are retired (config-side reverts owned by bd-b4cc45); if a node still hangs on a stale flake eval-cache, the per-node fix is a nixos-rebuild switch / eval-cache clear. The bd-2bbb1c queue substituter-pin note and its eval-cache caveat were reworded off the ms-mac-specific framing.
- Verified-clean of stale framing; repo flake confirmed clean by peers (sonance/ms-dev/caco-web-md2-0) on fresh main.
- NOT edited (out of lane, left to bd-109ab3 ctrl half / bd-b4cc45): `.cacophony/automation.yaml` NIX_CONFIG override + comments; `.cacophony/profiles/caco-macos.md` redhill mention.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `AGENTS.md`, `README.md`, `deploy/aca/README.md`, `deploy/aks/PRODUCTION-ROLLOUT.md`.
- Behavioural delta: documentation only.

## Operator-takeaway

The cache docs now match the live ms-dev/ms-dev-2 caches; the only remaining
redhill references are the automation.yaml override + caco-macos profile comment,
owned by the bd-b4cc45 / bd-109ab3 config-comment cleanup lanes.
