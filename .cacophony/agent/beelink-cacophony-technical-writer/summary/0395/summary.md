# Technical-writer review summary

## Goal

Retire the ms-mac eval-cache caveat in README/deploy now that it is resolved
(bd-880d4c closed), matching md2-1's AGENTS.md caveat-lift, so the cache docs are
consistent on the settled state.

## Bead(s)

- `bd-109ab3` — technical-writer/ctrl cache comment+doc cleanup (README/deploy slice).
- `bd-c63005` — technical-writer documentation maintenance.
- Documents bd-880d4c (ms-mac eval-cache resolved); context bd-716f8e (inherit-host default, AGENTS.md half by md2-1).

## Before state

- README.md (L317/L845) + deploy/aca/README.md carried the restored ms-mac eval-cache caveat ("keep the ms-mac override / avoid queued cargo on ms-mac until the eval-cache refresh is confirmed"). That confirmation arrived: Harry's nix-daemon restart dropped the in-memory redhill and msm-1's override-less queued-cargo verify confirmed both layers clean → bd-880d4c CLOSED. md2-1 lifted the AGENTS.md caveat (61b17fc31); caco-macos.md lifted by its owner.

## After state

- README/deploy now state ms-mac's eval-cache was resolved 2026-06-23 by an operator nix-daemon restart + override-less verify (bd-880d4c closed), so the ms-mac override and avoid-queued-cargo caveat are retired. Straggler guidance genericized (no transient node/file naming, aurora mid-rebuild): a node still resolving redhill is cleared at the right layer — nixos-rebuild for a dirty /etc/nix substituter line, or nix-daemon restart for a clean-config-but-stale daemon. README/deploy carry no bd-2bbb1c queue-default prose (AGENTS.md-only), so no inherit-host flip needed there.
- Verified caveat fully removed; validate-pages passed.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `README.md`, `deploy/aca/README.md`.
- Behavioural delta: documentation only.

## Operator-takeaway

The redhill cache docs reflect the fully-resolved state (ms-mac eval-cache fixed,
caveat retired) consistently across AGENTS.md + README + deploy; only aurora's
in-progress nixos-rebuild remains (bd-d1e90c).
