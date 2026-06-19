# Session summary — Linux build/test toolchain investigation for iOS/macOS

## Goal

Per Harry's operator directive (relayed by helsinki cluster-ctrl while the
single Apple-capable node ms-mac is offline in transit), determine what
iOS/macOS CODE and TEST work can run on a Linux toolchain so progress does not
stall on the one Apple node — while keeping RELEASE (Apple builds, simulator,
signing, notarization, App Store/TestFlight) on ms-mac. Deliverable: a grounded
analysis doc mapping the Linux-vs-macOS boundary and defining a Linux-runnable
code+test lane.

## Bead(s)

- `bd-3b7595` — [operator-directed] Linux build/test toolchain for iOS/macOS to
  reduce ms-mac dependency. Claimed by me (caco-tui-md2-1) under the operator
  clearance for ms-dev/ms-dev-2 dev agents; investigative Rust/Nix/CI/build work
  squarely in my competence despite my text-TUI specialism.
- related: `bd-1a082f` (caco-ios profile + build parity).

## Before state

- Apple-capable node ms-mac offline; iOS/macOS code progress at risk of
  stalling. No single doc mapping which companion build/test steps are
  Linux-feasible vs macOS-required.
- Known infra: docs/darwin-cross-builds.md (CLI Darwin cross-build via remote
  Nix Darwin builder); companion/ios + companion/macos SwiftPM packages; shared
  Rust core (caco-picophony / caco-sdk); three companion CI workflows.

## After state

- New investigation doc `docs/design/ios-macos-linux-toolchain.md` (164 lines)
  with: a TL;DR boundary table; evidence sections (shared Rust core, the
  Foundation-only Swift kits, the bd-4efbe9 nix-swift caveat, current CI); a
  darwin-cross app/test feasibility assessment; a concrete proposed Linux
  code+test lane; interim routing for Linux dev agents; and open questions.
- Key findings: (1) shared Rust pico/FFI core is Linux-native today; (2) both
  companion Swift kits (CacophonyCompanionKit / CacophonyKit) are Foundation-only
  (verified 0 UIKit/SwiftUI/AppKit imports) and a designed nix-swift/non-Darwin
  fallback already exists, so a Linux Swift smoke is feasible pending one
  validation; (3) app build/sign/release stay macOS-only and the darwin-cross
  path relocates but does not remove that requirement.

## Diff summary

- Code/content commit: c55e694166 (final landed squash SHA from the
  reintegration receipt). Summary artefact commit intentionally omitted.
- Files touched: docs/design/ios-macos-linux-toolchain.md (new).
- Tests: none (documentation/analysis deliverable; source-only validated —
  git diff --check clean, no AGENTS/CLAUDE churn). The one empirical uncertainty
  (swift build of the kit on Linux) is flagged as a follow-up rather than forcing
  a heavy Swift toolchain realization on a loaded shared host.
- Behavioural delta: none (docs only). Provides a routing/lane map for the fleet.

## Operator-takeaway

The fleet can keep iOS/macOS CODE moving on Linux without ms-mac for two layers:
the shared Rust pico/FFI core (already Linux-native) and the Foundation-only
Swift value-type kits (CacophonyCompanionKit/CacophonyKit) via a nix-swift smoke
— the latter gated only by one unvalidated question (does `swift build` succeed
on Linux given the package's Apple-only platforms decls; the bd-4efbe9 wrapper
bug was macOS-host-specific). App build / Simulator / sign / notarize / release
remain macOS-only; the darwin-cross mechanism only relocates that work to a
Darwin builder, it does not eliminate it. Next concrete step: a one-time Linux
`swift build` validation of the kit, then wire the Rust-core + kit-smoke +
parse-check lane.
