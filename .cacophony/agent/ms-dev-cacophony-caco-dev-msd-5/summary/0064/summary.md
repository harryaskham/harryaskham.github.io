# Session summary — bd-3e7ef2 ambient narration CLI config updates

## Goal

Address `bd-3e7ef2`: update ambient narration config in place from CLI enable/disable requests after metadata exists. TUI and TTS playback remain out of scope.

## Changes

- Added `AmbientNarrationCliConfigUpdate` and `apply_ambient_narration_cli_config_update(...)`.
- The update helper mutates an `AmbientNarrationConfig` in place for enable/disable requests, applies optional cadence, normalizes verbosity (`terse` -> `compact`, unknown -> `normal`), clamps cadence to at least 1, and returns the existing status projection with optional reason.
- Added dispatch for `caco ambient-narration enable` and `caco ambient-narration disable`.
- Dispatch now applies the config update helper and renders existing status/control-panel text or JSON.
- Added daemon pure-helper and CLI dispatch regressions.

## Validation

- `cargo test -p caco-daemon --lib apply_ambient_narration_cli_config_update_mutates_config_bd_3e7ef2 -- --test-threads=1`
- `cargo test -p caco-cli --lib ambient_narration_enable_disable_dispatch_updates_config_bd_3e7ef2 -- --test-threads=1`
- `git diff --check`

## Diff summary

- Code/content commit: `8b6404d2a5`
- Summary artefact commit: omitted intentionally; reintegration receipt is the source for the final landed squash SHA.
