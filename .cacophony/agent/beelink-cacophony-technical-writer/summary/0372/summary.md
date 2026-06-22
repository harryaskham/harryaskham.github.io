# Technical-writer summary — flip cacheless docs to resolved (host configs cleaned)

## Goal

Flip the interim cacheless-Nix docs to the resolved state. Both controllers
(ms-dev-ctrl, ms-dev-2-ctrl) confirmed the host nix cleanup landed on the main
dev/build nodes (ms-dev / ms-dev-2 nixos-rebuild switches; ms-mac on Determinate):
the dead ACA Attic (`redhill`) substituter is removed and the full public
substituter set (cache.nixos.org + cachix caches) restored. So plain nix /
queued validation now works normally and faster — the `cache.nixos.org`-only
override is no longer needed and is now slower (it drops the cachix caches).

## Bead(s)

- No implementation bead — operator/controller-directed documentation accuracy
  flip (technical-writer maintenance). This is the third and (per the controllers)
  stable revision of the cacheless guidance: bare `''` → `cache.nixos.org` override
  → resolved (plain nix, override as narrow stopgap).

## Before state

- AGENTS.md (L191 canonical ACA bullet + Dev Workflow pointer), README "Build and
  Run", and deploy/aca/README.md all prescribed the `--option substituters
  'https://cache.nixos.org'` override as the interim workaround, referencing a
  planned atticd. That guidance is now harmful on cleaned nodes (drops cachix).

## After state

- All four surfaces flipped to the resolved framing: the redhill substituter has
  been removed from the main dev/build node nix configs and the full public set
  restored, so plain `nix develop` / `nix build` / queued validation works normally
  and faster — no override needed on cleaned nodes. The `cache.nixos.org` override
  is documented as a NARROW STOPGAP only for any node still hanging on the dead
  redhill cold-realize (host config not yet cleaned), and the durable fix is the
  full multi-cache host config rather than a per-command override. Confirmed: no
  stale interim phrasing ("until atticd lands", "fleet-robust variant") remains.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `AGENTS.md` (×2), `README.md`, `deploy/aca/README.md`. No `docs/`
  HTML siblings. Verified no AUTOGEN/previous-summary churn in AGENTS.md.
- Tests: n/a (docs-only).
- Behavioural delta: documentation only.

## Operator-takeaway

The cacheless guidance now reflects the resolved state (plain nix is the default;
the override is a narrow stopgap only for any not-yet-cleaned node). Asked the
controller a scope question before flipping to avoid stranding an uncleaned node;
ms-dev-2-ctrl authoritatively confirmed the framing (main nodes cleaned, override
stopgap-only). Cacheless docs arc closed: bare `''` → cache.nixos.org → resolved.
