# Session summary — constrain macOS CI cargo jobs

## Goal

Fix `bd-31c0c6`, reducing CPU saturation from macOS CI cargo work on the shared `ms-mac` self-hosted runner.

## Bead

- `bd-31c0c6` — Reduce macOS CI cargo core usage on ms-mac runner

## Work performed

- Audited GitHub Actions paths that execute on self-hosted macOS runners.
- Updated release/dev/hourly native macOS binary matrix jobs to:
  - emit `CARGO_BUILD_JOBS=2` into `$GITHUB_ENV`,
  - log `macOS cargo concurrency: CARGO_BUILD_JOBS=2`,
  - pass both `CARGO_BUILD_JOBS` and explicit `cargo build --jobs 2` through `nix develop .#ci-release`.
- Updated the manual/tag `ci.yml` self-hosted macOS app job to:
  - set `CARGO_BUILD_JOBS=2`, `NIX_BUILD_CORES=2`, and `NIX_CONFIG: cores = 2`,
  - log the constrained cargo/Nix settings in runner diagnostics,
  - pass `--cores "${NIX_BUILD_CORES}"` to the direct `nix build .#cacophony-macos-app` step.
- Updated `README.md` and `AGENTS.md` to document that self-hosted macOS native cargo CI advertises and enforces the constrained cargo parallelism.

## Validation

Passed:

- PyYAML parse of `.github/workflows/ci.yml`, `dev.yml`, `hourly.yml`, and `release.yml`.
- Source assertion script confirming:
  - release/dev/hourly workflows contain the constrained macOS cargo job step,
  - `CARGO_BUILD_JOBS=2` is present,
  - native macOS cargo builds pass `--jobs "${CARGO_BUILD_JOBS:-2}"`,
  - `ci.yml` macOS app job carries `CARGO_BUILD_JOBS`, `NIX_BUILD_CORES`, and `--cores`.
- `git diff --check`.

## Outcome

The self-hosted macOS CI paths now advertise and apply two-job cargo/Nix concurrency limits on `ms-mac`, while Linux and cross-compile matrix jobs keep their existing behavior.
