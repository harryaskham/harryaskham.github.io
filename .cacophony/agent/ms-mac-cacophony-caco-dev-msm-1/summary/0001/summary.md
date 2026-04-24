# Session summary — Nix build for the macOS app (bd-aa8a1a)

## Goal

Wire `companion/macos/` into the top-level Nix flake so the macOS
native app can be built and smoke-tested hermetically with a single
command, on every developer's machine and in CI, without depending
on Xcode being on PATH.

## Bead(s)

- `bd-aa8a1a` — Configure Nix builds for entire macOS app stack.
- (parent: `bd-6d67e0` — Implement native macOS app with liquid
  glass design.)
- (precedes: `bd-ff6982` (CI), `bd-5cded9` (release artifacts).)

## Before state

- `nix build .#cacophony-macos-app` did not exist.
- `flake.nix` only built `caco` and `tmux-cli`; nothing knew about
  `companion/macos/`.
- Developers had to invoke `nix shell --inputs-from .. nixpkgs#swift
  nixpkgs#swiftpm -c swift build` by hand from inside the package
  directory to build anything.

## After state

- `nix build .#cacophony-macos-app` works on `aarch64-darwin` and
  `x86_64-darwin`.
- The derivation:
  - builds `Sources/CacophonyKit`, `Sources/Cacophony`, and
    `Sources/CacophonyKitSmoke` under the nixpkgs `swift` +
    `swiftpm` toolchains;
  - runs `swift run CacophonyKitSmoke` in `checkPhase` (8 / 8
    green inside the sandbox);
  - installs `result/bin/Cacophony`,
    `result/bin/CacophonyKitSmoke`, and a minimal
    `result/Applications/Cacophony.app/` bundle with a real
    `Info.plist` (CFBundleIdentifier `com.cacophony.macos`,
    `LSMinimumSystemVersion 14.0`, version pulled from
    `cargoVersion`).
- Linux + non-darwin systems are unaffected: the package is gated
  behind `pkgs.lib.optionalAttrs pkgs.stdenv.isDarwin`.
- `companion/macos/README.md` and `docs/macos-development.md` §8
  document the new `nix build .#cacophony-macos-app` path.

## Diff summary

- Files touched:
  - `flake.nix` — added `cacophony-macos-app` derivation (darwin-only
    via `optionalAttrs stdenv.isDarwin`).
  - `companion/macos/README.md` — Nix build documented as the
    primary path; bd-aa8a1a marked landed.
  - `docs/macos-development.md` — §8 "Quick start" updated.
- Tests: smoke-test target invoked inside `checkPhase` (8 checks);
  no new tests added.
- Behavioural delta: `nix build .#cacophony-macos-app` produces a
  hermetic, smoke-tested macOS app artefact. No Rust / non-darwin
  surfaces affected.

## Embedded artefacts

- (none — output is a sandbox-validated `result/` symlink and a
  signed-only-by-default `.app` bundle. Real signing / notarisation
  is bd-5cded9.)

## Operator-takeaway

Cold `nix build` is ~70s on M-series silicon (the Swift toolchain
download is the dominant cost on first run; subsequent builds are
~5-10s warm). The build deliberately produces a *minimal* `.app`
bundle (no signing, no notarisation, no embedded assets) so it can
run sandbox-only — the signed/notarised release artefact is
explicitly bd-5cded9's scope. This means CI (bd-ff6982) only needs
to invoke `nix build .#cacophony-macos-app` and check the exit
code; the smoke tests run as part of `checkPhase`. Linux runners
naturally skip the package because of the darwin gate.
