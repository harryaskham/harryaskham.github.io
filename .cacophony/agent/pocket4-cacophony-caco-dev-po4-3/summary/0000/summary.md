# Session summary — bd-c8e045 macOS development guide

## Goal

Stand up a comprehensive, developer-facing macOS build & run guide so
contributors picking up the in-progress native-macOS-app workstream
(epic bd-6d67e0 and sub-beads) inherit a coherent baseline of build
prerequisites, Nix vs Cargo paths, launchd lifecycle integration,
audio/STT/TTS specifics, and known macOS-only pitfalls — instead of
reverse-engineering platform behaviour from scattered SPEC and
flake.nix references.

## Bead(s)

- `bd-c8e045` — Update build instructions for macOS development
- (sibling: `bd-6d67e0` — [EPIC] native macOS app with liquid glass
  design; this guide reserves a stub section for that workstream)

## Before state

- Failing tests: none triggered by this bead (docs-only).
- Relevant metrics: zero macOS-specific developer doc in `docs/`;
  README mentioned macOS only obliquely (one-line install reference,
  one-line launchd/Darwin reference inside a 200+ word architecture
  paragraph). No `companion/macos/` exists yet; native app is
  pre-implementation.
- Context: developers landing on a Mac had to grep SPEC.md (>11k
  lines), flake.nix, and READMEs for `darwin`/`macos`/`launchd` to
  piece together the build path, audio permissions, and lifecycle
  shape.

## After state

- Failing tests: none.
- Relevant metrics: new `docs/macos-development.md` (~290 lines, 10
  sections) covering supported hosts, prerequisites, clone, Nix +
  Cargo build paths, common build issues, daemon/TUI run + launchd
  integration, audio/STT/TTS specifics, iOS companion pointer,
  native-app stub, troubleshooting checklist, related-doc index.
  README's `Documentation` section now links to it.
- Context: `bd-c8e045` AC ("comprehensive build and development
  instructions … prerequisites, build steps, running locally,
  debugging, troubleshooting … clear for new developers") satisfied
  via a single canonical doc with explicit pointers from README.

## Diff summary

- Files touched:
  - `docs/macos-development.md` (new)
  - `README.md` (one-line addition under `## Documentation`)
- Tests: +0 / -0 / flipped 0 (docs-only change; no compile or test
  surface impacted).
- Behavioural delta: no runtime change. Documentation surface adds
  one new file and one README link.

## Embedded artefacts

(None — docs-only bead, no runs to record.)

## Operator-takeaway

The native macOS app workstream (bd-6d67e0 cluster) now has a
landing page that already documents the *non-app-specific* macOS
build, lifecycle, and permission story (Xcode CLT, Nix flakes,
launchd `gui/$UID/com.cacophony.lifecycle`, microphone +
Accessibility prompts, Tendril macOS blocker bd-5c3937). When the
first sub-bead lands a buildable `companion/macos/` surface, the
contributor only needs to fill in the §8 "Native macOS app
(in progress)" stub — the surrounding scaffolding is already in
place.
