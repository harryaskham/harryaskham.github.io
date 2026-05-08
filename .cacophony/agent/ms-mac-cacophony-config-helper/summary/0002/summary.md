# Session summary — enable TTS stereo panning

## Goal

Enable operator-requested deterministic left/right stereo panning for Cacophony TTS so different agent speakers occupy stable positions in the stereo field. This was a small repo config change using the existing `speech.tts.voice_pan` support rather than new implementation work.

## Bead(s)

- `bd-19bb00` — Enable deterministic TTS stereo panning

## Before state

- Failing tests: none; this was an operator-directed configuration change.
- Relevant metrics: `caco config validate --project-config-dir=$(pwd)/.cacophony --json` passed before edit.
- Context: the codebase already supports `TtsConfig.voice_pan`, with deterministic sender hashing and post-synthesis WAV panning; no new schema or runtime code was required.

## After state

- Failing tests: none observed.
- Relevant metrics: `caco config validate --project-config-dir=$(pwd)/.cacophony --json` passed after edit.
- Context: `.cacophony/tts.yaml` now enables `voice_pan` with `spread: 0.8`, inherited through `values.tts` by global speech and TTS daemon speech configs.

## Diff summary

- Code/content commits: `9ebc55bff9` (agent branch; final landed squash SHA will come from reintegration receipt)
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `.cacophony/tts.yaml`
- Tests: +0 / -0 / flipped 0
- Behavioural delta: operator-audible TTS now applies deterministic per-agent stereo placement. It composes with effects because command-template effects still synthesize audio first and the existing panning stage applies during playback/post-synthesis.

## Operator-takeaway

No bead was needed for new functionality: panning already existed. The config now turns it on for the shared Cacophony TTS defaults with an audible but not hard-panned spread.
