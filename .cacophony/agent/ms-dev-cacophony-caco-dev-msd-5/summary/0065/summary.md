# Session summary — bd-cdf73c ambient narration toggle dispatch regression

## Goal

Address `bd-cdf73c`: pin CLI dispatch for ambient narration enable/disable after metadata and config-update helper exist. TUI and TTS playback remain out of scope.

## Changes

- Renamed the existing ambient narration enable/disable CLI dispatch regression to include `bd-cdf73c` coverage.
- The regression exercises `caco ambient-narration enable --cadence-secs 30 --verbosity verbose --json` and `caco ambient-narration disable --reason ...`, proving both commands dispatch to the config-update helper and renderer.

## Validation

- `cargo test -p caco-cli --lib ambient_narration_enable_disable_dispatch_updates_config_bd_3e7ef2_bd_cdf73c -- --test-threads=1`
- `git diff --check`

## Diff summary

- Code/content commit: `ad3ca41785`
- Summary artefact commit: omitted intentionally; reintegration receipt is the source for the final landed squash SHA.
