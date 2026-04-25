# Session summary — centralize Docker image build context manifest

## Goal

Eliminate the silent drift risk between the three places that
previously listed top-level repo paths to stage into the cacophony
Docker image build context: the canonical `Dockerfile` COPY
directives, `deploy/aca/deploy.sh`'s rsync invocation, and the
`aca-image` justfile target's rsync invocation.

## Bead(s)

- `bd-1129a0` — Centralize Docker image build context manifest

## Before state

- `Dockerfile` COPYs `Cargo.toml`, `Cargo.lock`, `crates/`, `plugins/`,
  `.cacophony/`.
- `deploy/aca/deploy.sh` hand-rolled the staging list:
  `.dockerignore`, `Dockerfile`, `Cargo.toml`, `Cargo.lock`,
  `flake.nix`, `flake.lock`, `deploy`, `crates`, `plugins`,
  `.cacophony` (correct as of bd-44acfb).
- `justfile` `aca-image` target hand-rolled a *different* list still
  referencing the obsolete top-level `configs` directory and missing
  `.cacophony` entirely. This was the latent divergence the bead
  flagged.
- No drift-detection test guarded any of the three surfaces.

## After state

- New `deploy/build-context.sh`: single source of truth exposing
  `cacophony_build_context_paths`, sourceable by shell callers and
  also runnable directly to print the manifest.
- `deploy/aca/deploy.sh` sources the manifest in
  `stage_image_context()`; the inline rsync list is gone.
- `justfile` `aca-image` target invokes `./deploy/build-context.sh`
  via command substitution; the obsolete inline list (and the stale
  `configs` reference) is gone.
- Three regression tests in `crates/caco/tests/deploy_assets.rs`:
  - `dockerfile_build_context_paths_match_manifest` asserts every
    top-level Dockerfile COPY source is listed in the manifest, and
    every manifest entry exists on disk.
  - `aca_deploy_script_uses_centralized_build_context_manifest`
    asserts deploy.sh sources the manifest and no longer references
    `configs`.
  - `aca_image_just_target_uses_centralized_build_context_manifest`
    asserts the justfile invokes `./deploy/build-context.sh` and no
    longer hand-rolls the inline list.
- `cargo test -p caco --test deploy_assets`: 89/89 passed.
- `cargo test-small`: 252/252 passed.
- `bash -n` on both shell files: clean.

## Diff summary

- Commit: `6b1d74b95` ("bd-1129a0: centralize Docker image build
  context manifest")
- Files touched (4 files, +242 / -11):
  - `deploy/build-context.sh` (new, +executable)
  - `deploy/aca/deploy.sh` (replace inline rsync list with
    sourced manifest)
  - `justfile` (replace inline rsync list with command substitution)
  - `crates/caco/tests/deploy_assets.rs` (+3 regression tests + 2
    helper functions)
- Tests: +3, 0 removed, 0 flipped.
- Behavioural delta: build context staging output is unchanged for
  `deploy/aca/deploy.sh` (the manifest is identical to what the
  script previously hardcoded). The `justfile` target now stages
  the *correct* set (adding `.cacophony/`, dropping obsolete
  `configs`); this means `just aca-image` was previously building
  an image that was missing the `.cacophony/` overlay and would
  not work in production. The fix is therefore both a drift guard
  and a latent bug fix for the justfile path.

## Embedded artefacts

(none)

## Operator-takeaway

Three callers, three lists, zero tests — the justfile had silently
drifted out of sync (still referencing the obsolete `configs/` path
and missing `.cacophony/` entirely), so `just aca-image` was building
an image that could not actually run. Centralizing into one shell
manifest and adding a Dockerfile-COPY-vs-manifest drift test means
the next person to add a top-level path gets a clear error pointing
at the manifest, not a mysterious runtime failure when the image is
finally deployed.
