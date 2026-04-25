# Session summary — TTS trace audibility evidence

## Goal

Respond to Harry's repeated ms-mac TTS concern by making `terminal=played` traces prove more than a successful playback return: they now include synthesized-audio duration and level metrics so silence can be distinguished from non-silent audio sent to the output backend.

## Bead(s)

- `bd-c7619f` — ms-mac TTS reports terminal=played local-device but operator hears no audio

## Before state

- Failing tests: none known for this scope; unrelated clippy breakages were already owned by other agents and were not duplicated.
- Relevant metrics: live TTS status was unmuted, queue-draining, `output_routing=local-device`, and recent traces showed `outcome=played sink=local-device detail=device=MacBook Pro Speakers`, but the trace could not prove whether the synthesized WAV contained non-silent samples.
- Context: `bd-d6033d` had already made macOS traces report the CoreAudio default device, but Harry still reported no audible output, so the next diagnostic gap was sample-level audibility evidence.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: targeted tests passed for non-silent WAV RMS/peak reporting, silent WAV negative-infinity reporting, and detail-string formatting; `cargo check -p caco-tui` and `cargo check -p caco-cli --tests` passed.
- Context: completed playback details now include duration, sample count, channels, sample rate, RMS dBFS, and peak dBFS. macOS traces still include the output device, and Pulse/local-device paths also emit the level metrics.

## Diff summary

- Commits: `c6ff717ee`
- Files touched: `crates/caco-tui/src/playback.rs`, `SPEC.md`, `README.md`
- Tests: +3 focused playback metric tests.
- Behavioural delta: `caco tts trace` terminal `played` events can now show whether a clip was non-silent and long enough to hear, rather than only proving the playback API returned success.

## Operator-takeaway

This does not by itself let software prove Harry physically heard the speakers, but it closes the main diagnostic blind spot: future ms-mac `played` traces will name the output device and include RMS/peak evidence for the actual synthesized clip.
