# Session summary — macOS app build loop now defaults to daemon-friendly limits

## Goal

Reduce the risk that native macOS Swift/Nix app builds starve the live daemon
on ms-mac during operator or visual-QA loops. The goal was to make the default
operator-facing build/test/install path conservative by default and to document
an explicit lightweight validation loop that should be used before heavyweight
packaging/install steps.

## Bead(s)

- `bd-af5559` — [macOS build] Resource-limit Swift/Nix app builds so they do not starve the daemon

## Before state

- The native macOS app recipes existed, but only `macos-app-build` visibly
  constrained Nix/Swift jobs.
- The docs still encouraged direct heavyweight build flows without clearly
  prioritizing a low-impact validation loop for live ms-mac nodes.
- On an operator-used ms-mac host, repeated SwiftPM/Nix app builds could compete
  with the daemon and TTS work for CPU / scheduler time.

## After state

- `just macos-app-test` now runs under lower priority via `nice` and uses the
  same conservative Nix core/max-job defaults as the build path for the smoke
  runner.
- `just macos-app-build` now also runs under lower scheduler priority via
  `nice -n ${CACO_MACOS_BUILD_NICE:-10}`.
- The macOS docs now steer operators toward the safer sequence:
  1. `just macos-app-test`
  2. `just macos-app-build`
  3. `just macos-app-install`
  4. package/UI acceptance only after the light loop is green
- `docs/macos-development.md` and `companion/macos/README.md` both document the
  conservative defaults:
  - `CACO_MACOS_SWIFT_JOBS=1`
  - `CACO_NIX_CORES=2`
  - `CACO_NIX_MAX_JOBS=1`
  - `CACO_MACOS_BUILD_NICE=10`
- The docs also give a direct constrained SwiftPM smoke command for cases where
  an operator wants the underlying command instead of the `just` recipe.

## Diff summary

- Files touched:
  - `justfile`
  - `docs/macos-development.md`
  - `companion/macos/README.md`
- Validation:
  - `just --list` confirms the macOS app recipe surface remains intact
  - manual doc/readback review of the updated macOS build sections
- Behavioural delta:
  - app builds/tests are less likely to monopolize ms-mac by default
  - operators now have a documented “smoke first, package later” loop for live
    daemon-backed nodes

## Operator-takeaway

This is a default-path safety fix, not a packaging-architecture rewrite. The
macOS app build/install commands now bias toward low parallelism and lower CPU
priority, and the docs make the lightweight validation loop the first-class
path. That should reduce daemon starvation on ms-mac during ordinary operator
use without blocking heavier release/package workflows when explicitly needed.
