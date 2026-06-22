# Session summary — bd-3b2b2e: source TUI Daemon-Lang cycle pool from configured speech.tts.lang

## Goal

Make the TUI "Daemon Lang" cycle control step through exactly the languages the
operator declared in `speech.tts.lang`, instead of a fixed built-in preset list —
fulfilling the intent of the `tts.lang` config option that bd-8ad093's selector
did not yet honor.

## Bead(s)

- `bd-3b2b2e` (P3, feature) — Source TUI Daemon-Lang cycle pool from configured speech.tts.lang. Self-directed pick in the caco-tui lane (ctrl de-gated a spurious operator-action label).

## Before state

- `SpeechState::daemon_lang_cycle_pool()` (caco-tui/speech.rs) always built the cycle pool from the hardcoded `DAEMON_LANG_PRESETS` (en-US/en-GB/es-ES/…), folding in the active lang.
- The operator's configured `speech.tts.lang: Option<Vec<String>>` was ignored by the cycle pool.
- Tests: 2 existing daemon_lang_cycle_pool tests (presets-first, fold-in).

## After state

- `daemon_lang_cycle_pool()` prefers the configured `speech.tts.lang` list (declared order) when non-empty, else falls back to `DAEMON_LANG_PRESETS`, still folding in the active lang.
- Tests: 3 daemon_lang_cycle_pool tests (added `..._prefers_configured_list_bd_3b2b2e` covering configured-list-preferred, active-lang fold-in, empty-list fallback). All pass (tj-0378bc67).
- Validation: queued `cargo check --workspace --tests` green (tj-0b0b4cff) — no cross-crate break.

## Diff summary

- Code commit: `e6c799122` (will be the landed squash SHA from the reintegration receipt).
- Files touched: `crates/caco-tui/src/speech.rs` (single file, 53 insertions).
- Approach: added `SpeechState.configured_langs: Vec<String>`, populated config-direct in `from_speech_config` + `apply_speech_config_live` (mirroring `voice_entries` from `tts.voices`). Chose config-direct over the bead's suggested daemon-status-route AND the AudioCapabilities-field route — it avoids the AudioCapabilities ~43-site exhaustive-construction fan-out (ctrl's concern) AND a TTS-daemon round-trip; `from_speech_config` already reads `tts.*` so `tts.lang` was right there. No construction fan-out (only the Default impl + struct updated).
- Tests: +1 (3 total daemon_lang_cycle_pool tests).
- Behavioural delta: TUI daemon-lang cycle honors configured `speech.tts.lang`; preset behavior preserved when unset.

## Operator-takeaway

The TUI language cycle now reflects exactly the languages you configured in `speech.tts.lang`. The implementation deliberately read the config directly in `SpeechState::from_speech_config` (same path that already loads configured voices) rather than round-tripping through the TTS daemon status — simpler, single-file, and side-steps the AudioCapabilities construction fan-out that the bead flagged as a hazard.
