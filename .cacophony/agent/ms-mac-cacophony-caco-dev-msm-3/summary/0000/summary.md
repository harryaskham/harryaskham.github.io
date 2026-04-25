# Session summary — TTS setting missing-value wording

## Goal

This session improved missing-value errors for TTS runtime-setting commands so operators get immediate guidance for voices, speed values, models, and output routing modes.

## Bead(s)

- `bd-e06408` — [CLI polish] tts setting commands missing-value discoverability wording

## Before state

- Failing tests: no exact regression covered missing values for `caco tts set-voice`, `set-speed`, `set-model`, or `tts io output set`.
- Relevant metrics: those commands emitted bare required-argument errors.
- Context: this was a cohesive CLI polish slice taken while the normal implementation queue contained only permanent trackers.

## After state

- Failing tests: none in scoped validation before replay.
- Relevant metrics: `cargo test -p caco-cli tts_setting_commands_missing_values_use_discoverability_hints --lib` and `cargo check -p caco-cli --lib` passed before replay; `cargo test-small` hung twice without leaving live cargo processes, so the focused regression is rerun after replay.
- Context: TTS missing-value errors now point to voices/config/status or list allowed values inline.

## Diff summary

- Commits: `c78251fde`
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: added exact regression `tts_setting_commands_missing_values_use_discoverability_hints`.
- Behavioural delta: four TTS runtime-setting command errors now provide actionable hints instead of bare required-argument text.

## Operator-takeaway

TTS runtime-setting commands now tell operators where to find valid values or what range/modes are expected when a required value is missing.
