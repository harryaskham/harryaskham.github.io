# Session summary — bd-68bde8 caco audio speak voice-filter post-processing

## Goal

Wire the existing sox-based voice-filter pipeline (already used by
`caco msg speak` via the playback-side TTS daemon loop) through the
direct `/api/v1/audio/speech` daemon endpoint so a caller can request
filtered audio in a single round-trip via `caco audio speak
--voice-filter <preset>`.

## Bead(s)

- `bd-68bde8` — [bd-45ea63 follow-up] caco audio speak --voice-filter
  post-processing

## Diff summary

- `crates/caco-daemon/src/audio.rs`: +`apply_voice_filter_preset` and
  `run_sox_filter` helpers; +`voice_filter: Option<String>` on
  `SpeechRequest`; call site in `handle_speech` between bytes-received
  and size/event emission; 9 existing `SpeechRequest` literals in
  handlers + tests updated to `voice_filter: None`; 3 new unit tests
  for the preset helper (none / unknown / non-WAV passthrough).
- `crates/caco-cli/src/lib.rs`: +`--voice-filter` arg spec on
  `caco audio speak`; threaded through dispatch into
  `dispatch_audio_speak` and `build_audio_speech_request_body`; 1 new
  unit test pinning voice-filter-only-mode + 2 existing tests updated
  for the new helper signature.
- Behavioural delta: `caco audio speak --voice-filter <preset> --text
  "..."` returns sox-filtered WAV from the daemon in a single round
  trip; soft-fails to unfiltered audio on missing sox / non-WAV /
  timeout / unknown preset.
- Tests: +4, all pass. 104 daemon `audio::tests::*` pass; cargo
  test-small green; clippy clean.

## Before state

- `caco audio speak` had `--speed` and `--instructions` from the
  bd-45ea63 slice, but no way to request a sox preset filter.
- The sox preset effect chain lived in `caco-tui::voice_filter`,
  unreachable from `caco-daemon` because the `tui → daemon` dep
  direction blocks the reverse.
- `caco_config::VoiceFilterPreset::to_sox_args` already exposed the
  per-preset effect chain in a crate that the daemon does depend on.

## After state

- New `voice_filter: Option<String>` field on `audio::SpeechRequest`,
  serde-default so existing callers and the daemon's other internal
  call sites are unaffected (all 9 remaining `SpeechRequest` literals
  in tests/handlers updated to set `voice_filter: None`).
- New daemon-internal helpers in `crates/caco-daemon/src/audio.rs`:
  - `apply_voice_filter_preset(audio_bytes, preset_name)` — resolves
    the preset via `caco_config::VoiceFilterPreset::from_name`,
    bails out with a soft warning on `none` / unknown preset / non-WAV
    input / sox failure, and returns unfiltered bytes in all failure
    paths.
  - `run_sox_filter(audio_bytes, effects)` — minimal sox runner: pipes
    bytes through `sox -t wav - -t wav - <effects>` with a 10s
    deadline; kills the child on timeout. No daemon-host dependency on
    the caco-tui crate.
- Wired into `handle_speech` after the upstream provider returns audio
  bytes and before the response headers are built, so `size_bytes` and
  the `speech_finished` UI/feed events reflect the filtered output.
- New `--voice-filter` flag on `caco audio speak` (CLI arg spec, dispatch
  arg, JSON body forwarding via `build_audio_speech_request_body`).
- Tests added:
  - daemon-side: `apply_voice_filter_preset` returns input unchanged for
    `none`, unknown preset, and non-WAV input — deterministic on any
    host because no sox invocation is reached.
  - cli-side: `build_audio_speech_request_body` now omits `voice_filter`
    when caller passes `None` and forwards the preset name when set;
    extra `audio_speak_request_body_voice_filter_only` test pins the
    isolation of the new field from the existing speed/instructions
    overrides.
- All 104 daemon `audio::tests::*` pass; 3 new cli tests pass; cargo
  test-small green; clippy clean (preexisting build-script warning
  unchanged).

## Soft-fail policy (matches caco-tui pipeline)

- Missing sox → unfiltered audio + eprintln warning, no error to caller.
- Non-WAV format (mp3/opus/etc) → unfiltered audio + warning.
- sox timeout (10s) → kill child, return unfiltered audio.
- Unknown preset name → unfiltered audio + warning. `none` is the
  documented opt-out.

## Out of scope for this bead

- Rotation/config-driven filter selection (already happens in `caco msg
  speak` at the playback side; not relevant for the synchronous
  audio/speech endpoint where the caller controls the preset).
- SSML express-as block (bd-5b199b).
- Migrating `caco-tui::voice_filter` into a shared crate — the daemon's
  needs are met by the existing `VoiceFilterPreset::to_sox_args` in
  caco-config plus a small inline runner. Crate-extraction can wait
  until a third caller appears.

## Operator-takeaway

`caco audio speak --voice-filter telephone --text "..."` now returns
filtered WAV audio in one call, without the playback-side TTS daemon
detour. Filter coverage matches the existing 10 presets (telephone,
hall, lofi, underwater, radio, whisper, cave, megaphone, robot,
vintage). Soft-fails to unfiltered audio on any sox unavailability so
deployment regressions are warning-level, not breakage.
