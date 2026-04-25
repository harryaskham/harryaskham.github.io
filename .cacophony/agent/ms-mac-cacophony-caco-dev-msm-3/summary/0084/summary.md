# Session summary — macOS TTS playback trace evidence

## Goal

Respond to the repeated operator directive to keep ms-mac healthy and ensure the ms-mac TTS daemon is actually playing audio. The concrete fix was to close the observability gap where `caco tts trace` could report `outcome=played` after a rodio sink drained without identifying which CoreAudio output device was bound.

## Bead(s)

- `bd-d6033d` — tts: `outcome=played` is not proof of audible playback (rodio sink.empty() race)

## Before state

- Failing tests: none known for the touched code before implementation.
- Relevant metrics: ms-mac `caco status` showed `caco-daemon` and `caco-tts-daemon` running; `caco tts status` showed unmuted, `local-default`, voice `Mai-Voice-Finetuned-1:M`, and queue activity. macOS output volume was 31 and not muted. Existing traces included several `outcome=played sink=local-default` events, but no device name.
- Context: multiple ms-mac agents could prove daemon/mute/routing state and trace terminal events, but none could physically hear the Mac. The remaining gap was distinguishing “rendered to the listened-to CoreAudio device” from “rodio consumed frames but macOS routed them elsewhere.”

## After state

- Failing tests: none in scoped validation.
- Relevant metrics: `cargo test -p caco-tui playback_result` passed; `cargo check -p caco-cli --tests` passed.
- Context: macOS playback now resolves the CoreAudio default output device before binding rodio, waits with `sink.sleep_until_end()` instead of polling `sink.empty()`, and returns backend detail such as `device=MacBook Pro Speakers` for TTS trace terminal events.

## Diff summary

- Commits: `f8012083f` (playback trace/device change; this summary is committed as sibling session-recording commits)
- Files touched: `crates/caco-tui/src/playback.rs`, `crates/caco-cli/src/lib.rs`
- Tests: targeted caco-tui playback-result tests and caco-cli test-target compile check.
- Behavioural delta: `outcome=played` can now carry playback backend detail in `caco tts trace`, and playback failures/interruption from the handle are reflected as `failed`/`interrupted` terminal outcomes rather than always being logged as played after `handle.wait()` returns.

## Operator-takeaway

The ms-mac TTS daemon was running and unmuted, but the old trace was too weak to prove the operator-listened output device. This change makes future TTS probes materially more useful by showing the CoreAudio device that rodio actually bound when it reports playback completion.
