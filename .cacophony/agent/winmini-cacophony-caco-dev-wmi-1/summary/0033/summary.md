# Session summary — flake.nix version derived from Cargo.toml (bd-c6bf88)

## Goal

flake.nix had hardcoded `version = "1.2.3"` while the workspace
was at 1.2.493. Release process forgot to bump it.

## Bead(s)

- `bd-c6bf88` — Our release process is not updating flake.nix
  package versions (P2 task)

## Before state

- `pname = "caco"; version = "1.2.3";` hardcoded in flake.nix.
- Required manual sync on every release; drifted by ~490 patch
  versions.

## After state

- New `cargoVersion` let-binding at top of flake outputs reads
  `./Cargo.toml` and extracts the workspace version via
  `builtins.match`. Falls back to "0.0.0-unknown" if regex fails.
- `version = cargoVersion;` in cacoUnwrapped derivation.
- Verified: `nix derivation show .#packages.x86_64-linux.caco`
  contains `caco-1.2.493.drv` (current workspace version).

## Diff summary

- Files touched (+10 / −1):
  - `flake.nix`: cargoVersion let-binding + use in cacoUnwrapped.

## Verification

- `nix eval --impure --expr '...'` returns "1.2.493".
- `nix derivation show .#packages.x86_64-linux.caco` shows
  caco-1.2.493 in the source path.

## Operator-takeaway

Future releases automatically pick up the version from
Cargo.toml. No manual flake.nix bump required. Closes the
~490-version drift.
