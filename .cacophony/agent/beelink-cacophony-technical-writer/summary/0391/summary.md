# Technical-writer review summary

## Goal

Document the now-landed queued nix-develop substituter pin (bd-2bbb1c), the
cacheless-robust gate-hang guard I had tracked as a pending doc item.

## Bead(s)

- `bd-c63005` — technical-writer documentation maintenance.
- Documents bd-2bbb1c (pin queue nix-develop substituters); cross-links bd-888b1a (release-side cacheless nix build).

## Before state

- The AGENTS.md ACA-cache substituter note documented the host-config / eval-cache redhill posture but not the new queue-side `CACO_QUEUED_NIX_SUBSTITUTERS` pin (bd-2bbb1c) that landed at 6dd92aaf01.

## After state

- AGENTS.md (the canonical ACA-cache note at line ~191 plus the short summary at ~233) now documents: queued cargo jobs realize the dev-shell with `--option substituters https://cache.nixos.org` (public cache only) by default, so a dead/slow private substituter in a node's host `/etc/nix` config can no longer hang queued validation (the ~55min cold-store hang), matching the release-side bd-888b1a posture; `CACO_QUEUED_NIX_SUBSTITUTERS` disable-token/empty inherits host, a custom value keeps a private cache. Explicitly noted it is the queued `nix develop` pin only and does NOT clear the separate ms-mac flake eval-cache.
- Verified against crates/caco-daemon/src/queued_job_env.rs (DEFAULT_QUEUED_NIX_SUBSTITUTERS / queued_nix_substituters_from, bd-2bbb1c).

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `AGENTS.md`.
- Behavioural delta: documentation only.

## Operator-takeaway

Operators now have the queue-side substituter knob documented next to the host
substituter posture, with the precise boundary that it does not fix the ms-mac
eval-cache case.
