# Session summary — bd-b327cd TTS setter validation + format consistency

## Goal

Stop `caco tts set-voice / set-model / set-speed` from silently
mutating cluster TTS state with bogus values. Validate every setter
argument BEFORE the cluster-mutating HTTP call lands at the daemon,
using the daemon's own enumeration endpoints as the source of truth
so the allowed set never drifts from what the daemon supports.

## Bead(s)

- `bd-b327cd` — [CRITICAL UX] caco tts set-voice / set-model / set-speed accept ANY value with no validation; format inconsistency between setters

## Before state

- `caco tts set-voice --voice zzz-no-such-voice` returned `TTS daemon voice set to: zzz-no-such-voice` and the daemon happily persisted the bogus voice; cluster TTS would then fail on the next utterance.
- `caco tts set-model --model bogus-model-xyz` had the same accept-anything behaviour.
- `caco tts set-speed` parsed the value as f32 but did not bounds-check it; `--speed 9999` or `--speed -1` would silently land at the daemon.
- Format inconsistency: `set-model` fell into the JSON default arm (`{"model":"...","ok":true}`) while `set-voice` and `set-speed` returned `TTS daemon X set to: Y` text. Scripts and operators saw two different shapes from sibling setters.
- Failing tests: bd-c19193 (pre-existing, unrelated).

## After state

- New helpers in `crates/caco-cli/src/lib.rs`:
  - `validate_tts_speed(f32)` enforces `MIN_TTS_SPEED..=MAX_TTS_SPEED` (0.25..=4.0, the OpenAI audio.speech documented range) and rejects NaN/Infinity.
  - `validate_tts_voice(client, base, voice)` pre-fetches `/api/v1/tts/voices`, parses both object-form (`{name, source}`) and bare-string voice payloads via `collect_known_voice_names`, and refuses with `unknown TTS voice 'X' — available: a, b, c` when the value is not in the daemon's enumeration.
  - `validate_tts_model(client, base, model)` pre-fetches `/api/v1/tts/config`, parses `tts_models` / `models` / legacy single-string `model` shapes via `collect_known_model_names`, refuses with the same friendly error format.
- `dispatch_tts_control` calls the three validators before any state-mutating HTTP request, so a bogus value never reaches the daemon.
- Network-fetch failure during validation falls through to the dispatch path on purpose: a transient unreachable-daemon must not turn into a hard refusal that blocks recovery; the dispatch path itself surfaces the real connectivity error.
- `set-model` now returns `TTS daemon model set to: <model>` matching its siblings; `--json` still produces the structured payload for both.
- Failing tests: bd-c19193 (unchanged, pre-existing).

## Diff summary

- Commit: `e8e2f992 bd-b327cd: validate caco tts set-voice / set-model / set-speed before dispatch`
- Files touched: `crates/caco-cli/src/lib.rs` (+276 / -0).
- Tests: +10 / -0 / flipped 0
  - `bd_b327cd_speed_validation_accepts_documented_range`
  - `bd_b327cd_speed_validation_rejects_out_of_range`
  - `bd_b327cd_speed_validation_rejects_non_finite`
  - `bd_b327cd_known_voices_object_form`
  - `bd_b327cd_known_voices_string_form_tolerated`
  - `bd_b327cd_known_voices_missing_yields_empty`
  - `bd_b327cd_known_models_tts_models_field`
  - `bd_b327cd_known_models_models_fallback`
  - `bd_b327cd_known_models_single_model_legacy_shape`
  - `bd_b327cd_known_models_missing_yields_empty`
- Behavioural delta: bogus voice / model / speed values are refused at the CLI before any daemon call. Valid values flow through unchanged. Daemon-side behaviour is unmodified.

## Operator-takeaway

The footgun is closed: typo'd voices and models cannot break live
cluster TTS through `caco tts set-*` anymore. The validators use the
daemon's own enumeration so adding a new voice or model on the
daemon-side automatically widens the acceptable CLI set with no
duplication. The format-consistency change (set-model now emits
text by default) brings sibling setters into line; any caller that
was parsing the JSON default of set-model should switch to `--json`
explicitly for the structured payload.
