## Goal

Fix broken-on-main `just docs-build` (rc=3) caused by caco-docs-gen
requiring AUTOGEN sentinels in docs/profiles.html when the file still
uses legacy sentinels from `scripts/render-profiles-docs.py`.

## Bead(s)

- `bd-167bd6` — follow-up hotfix for graceful no-op on missing sentinels.

## Before state

`caco-docs-gen` returned exit code 3 (MissingSentinel) when run
against the on-disk `docs/profiles.html` which uses legacy
`BEGIN GENERATED SHIPPED PROFILES` markers. This broke `just docs-build`
for any agent rebasing onto main.

## After state

Binary now treats MissingSentinel as a graceful no-op (rc=0) with an
informational stderr message explaining that `scripts/render-profiles-docs.py`
remains the canonical generator. Also reverted the legacy-sentinel
auto-detection in `apply_shipped_profiles_section` that would have
silently rewritten the file with an incompatible layout. 1 new test
covers the error-variant contract the binary depends on.

## Diff summary

- `crates/caco-profile/src/bin/caco-docs-gen.rs`: MissingSentinel
  match arm returns rc=0 with informational message instead of rc=3.
- `crates/caco-profile/src/docs_gen.rs`: reverted legacy-sentinel
  auto-detect in `apply_shipped_profiles_section`; replaced with test
  `apply_shipped_profiles_section_errors_with_autogen_name_on_legacy_only_input`
  documenting the contract.

## Operator-takeaway

`just docs-build` no longer breaks on main. The Rust generator
gracefully defers to the python script when AUTOGEN sentinels are
absent. Full sentinel migration remains a future step.
