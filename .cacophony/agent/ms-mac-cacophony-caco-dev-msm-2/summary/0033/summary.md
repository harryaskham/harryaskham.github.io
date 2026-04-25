# Session summary — TTS local loopback proof helper

## Goal

Create a safe first-party procedure for ms-mac TTS hardware/audibility discrimination. Multiple agents could prove the Cacophony TTS daemon reached `played` on `local-device` / MacBook Pro Speakers, but none could physically hear the speaker from the runtime; this chunk adds a bounded microphone loopback helper and documents how to interpret it.

## Bead(s)

- `bd-c8361e` — Add microphone loopback proof for ms-mac TTS local-device audibility
- Related: `bd-fbf0ce` — [operator-action] Verify ms-mac TTS audible playback path

## Before state

- Failing tests: unrelated broken-on-main clippy/doc issues were owned by other agents; not touched here.
- Relevant metrics: manual `rec` + `/usr/bin/say` probe produced all-zero baseline and all-zero during-playback audio, proving the current agent process cannot use default input to verify acoustic output.
- Context: TTS route/status/trace evidence was healthy, but trace success alone could not satisfy the operator's repeated request for actual audible playback.

## After state

- Failing tests: none in the lightweight validation performed for this helper.
- Relevant metrics: `bash -n scripts/tts-local-loopback-proof.sh` passed; `scripts/tts-local-loopback-proof.sh --json` returned `ok=true`, `detected=false`, `all_zero_input=true`, with 2s baseline and 8s probe stats, matching the current default-input limitation.
- Context: docs now describe the helper, privacy properties, and the meaning of an all-zero result: no input signal detected, not proof of TTS daemon failure.

## Diff summary

- Commits: `70d367e8d`
- Files touched: `scripts/tts-local-loopback-proof.sh`, `docs/macos-development.md`
- Tests: +1 shell helper; no Rust tests.
- Behavioural delta: operators/agents have a repeatable, bounded, non-destructive command to emit a known macOS sound and summarize microphone/input amplitude without retaining audio by default.

## Operator-takeaway

The helper confirms the current ms-mac agent process cannot prove physical audibility through the default microphone path because captured input is all-zero. Cacophony TTS still shows local-device playback; actual speaker audibility still needs local listener confirmation or fixing input permissions/device selection for loopback proof.
