# Session summary — launchd service load diagnostics

## Goal

Fix `bd-2d3244`: the native lifecycle supervisor can be installed but not loaded after ms-mac restart, while first-party `caco service load/start/restart` left operators with launchd bootstrap exit 5 / kickstart exit 113 and insufficient diagnostics.

## Bead(s)

- `bd-2d3244` — Alert and repair when native lifecycle supervisor is installed but not loaded

## Changes

- Updated `crates/caco-cli/src/service_cmd.rs`:
  - added explicit launchd helpers for canonical plist path, not-loaded detection, and bootstrap/kickstart load steps;
  - made launchd `caco service start` / `restart` route installed-but-not-loaded supervisors through `caco service load` before a kickstart-only action;
  - added launchd-specific diagnostics for bootstrap exit 5 (`Input/output error`) and kickstart exit 113 (`service not found in gui/<uid>`), including plist validation and launchd-domain guidance;
  - included per-step diagnostics in `caco service load --json` and text output.
- Updated `SPEC.md`, `README.md`, and `AGENTS.md` to preserve the launchd load/start/restart contract and diagnostic expectations.
- Tightened the orphan-goal reintegration guard so persistent-agent embedded docs guard references such as `bd-20d2dc` are not mistaken for assigned bead goals when the actionable goal section is empty.
- Added focused unit coverage for the bd-2d3244 launchd not-loaded repair/diagnostic behavior and the embedded-docs guard false-positive case.

## Validation

- `rustfmt --edition 2021 --check --config skip_children=true crates/caco-cli/src/service_cmd.rs` — passed.
- `CARGO_BUILD_JOBS=2 cargo test -p caco-cli bd_2d3244 -- --test-threads=1` — passed (3 focused tests).
- `CARGO_BUILD_JOBS=2 cargo clippy -p caco-cli --lib --no-deps -- -D warnings` — passed.
- `git diff --check` — passed.

## Coordination notes

- Claimed `bd-2d3244` only after the local strict board gate turned green.
- Did not run raw launchctl/service-manager commands; implementation changes use the first-party `caco service` surface.
- Live `caco service status --json` before the code change showed `health_state=not_loaded` for `com.cacophony.lifecycle`, matching controller evidence.
