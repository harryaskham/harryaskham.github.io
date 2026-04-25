# Session summary — TTS audibility probe

## Goal

Add a first-party, bounded way to distinguish "the TTS daemon reported played" from "local speaker output was acoustically detectable" so future ms-mac audio incidents do not depend solely on Harry or another local listener being present.

## Bead(s)

- `bd-20f163` — ms-mac TTS: add microphone-loopback audibility check so agents can self-verify hardware playback without operator listening
- Related closed context: `bd-c8361e` — Add microphone loopback proof for ms-mac TTS local-device audibility

## Before state

- Failing tests: none known for this scope.
- Relevant metrics: ms-mac router probes were reporting local-device/MacBook Pro Speakers terminal `played` with non-silent RMS/peak metrics, but prior microphone attempts captured all-zero audio.
- Context: Operators could inspect daemon status, output routing, and trace metrics, but there was no canonical `caco tts` command that ran a bounded acoustic loopback probe and explicitly reported mic-permission/input failures as unverifiable rather than audible.

## After state

- Failing tests: none observed in targeted validation.
- Relevant metrics: added 3 focused unit tests for audibility classification (`detected`, all-zero unverifiable, below-threshold unverifiable); `cargo check -p caco-cli --tests` passed; `cargo test -p caco-cli tts_audibility_probe --lib` passed; `cargo run -q -p caco -- tts audibility probe --help` showed the new CLI surface.
- Context: Harry later confirmed the active ms-mac TTS route is healthy after the local-device fix, so this change remains a durable follow-up proof path rather than emergency TTS firefighting.

## Diff summary

- Commits: current HEAD for this summary chunk (`bd-20f163: add TTS audibility probe`)
- Files touched: `crates/caco-cli/src/lib.rs`, `SPEC.md`, `README.md`, `docs/macos-development.md`, `.cacophony/agent/ms-dev-cacophony-caco-dev-msd-5/summary/0000/summary.md`
- Tests: +3 focused unit tests / -0 / flipped 0
- Behavioural delta: `caco tts audibility probe` now records a short baseline, emits one explicit `caco msg speak`, records a short default-input sample, reports normalized amplitude stats, and classifies results as `audibility=detected` or `audibility=unverifiable` with explicit `reason=no-mic-permission-or-muted-input` for all-zero captures. Probe WAVs are deleted unless `--keep-audio` is passed.

## Operator-takeaway

The important distinction is now encoded in the product surface: daemon trace success and non-silent buffer metrics are useful, but they are not the same as acoustic audibility. The new probe gives agents a safe, short, privacy-bounded check and refuses to overclaim when macOS microphone permission or input routing makes audibility unverifiable.
