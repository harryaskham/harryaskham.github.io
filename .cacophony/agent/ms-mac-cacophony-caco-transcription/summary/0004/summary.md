# Session summary — native macOS live STT capture

## Goal

Continue the caco-stt-daemon rollout work by fixing the concrete capture failure observed on `ms-mac`: the configured `local-device` / CoreAudio input was being passed to the PulseAudio `parec` live-capture backend, causing repeated capture-child exits and preventing the ambient STT daemon from producing transcript diffs.

## Bead(s)

- `bd-930017` — Make configured ms-mac and sgu24 STT daemons produce transcripts

## Before state

- Failing tests: none initially; runtime health was failing instead.
- Relevant metrics: `caco stt daemon status --instance ms-mac` showed `process_alive=true`, `muted=false`, `capture_running=false`, zero transcript entries, and `last_error: capture child exited with exit status: 2`. `caco stt daemon logs --instance ms-mac` showed repeated `parec` failures such as `No such entity` / `closed stdout without exiting` for `--input local-device --device coreaudio_device`.
- Context: `caco @sgu24 stt daemon status --instance sgu24` still reported no pid/control port. Read-only SSH inspection showed the phone's checkout config was stale and did not yet contain the checked-in `caco-stt-daemon` service entries from the earlier config work.

## After state

- Failing tests: none in focused validation. One earlier focused test command (`tj-9b2f1604`) failed because I passed two Cargo test filters; corrected validation passed.
- Relevant metrics: queued `cargo check -p caco-cli --tests` passed (`tj-b72068ac`), focused `cargo test -p caco-cli live_capture --lib -- --test-threads=2` passed (`tj-7c3f2605`), queued `cargo build -p caco` passed (`bj-92bf2604`), `docs/validate-pages.sh` passed after updating the transcription HTML sibling hash, `rustfmt --edition 2021 --check crates/caco-cli/src/audio_cmd.rs` passed, and `git diff --check` passed.
- Context: the built patched binary can run a short native live STT smoke without the immediate `parec` failure path. The installed daemon on ms-mac still runs the pre-patch binary until this work is reintegrated/released, and sgu24 still needs config rollout/service convergence before its daemon appears.

## Diff summary

- Commits: `21e13d9f6`.
- Files touched: `Cargo.lock`, `SPEC.md`, `crates/caco-cli/Cargo.toml`, `crates/caco-cli/src/audio_cmd.rs`, `docs/transcription.md`, `docs/transcription.html`.
- Tests: +1 focused routing test / -0 / flipped 0.
- Behavioural delta: `caco audio transcribe --live` now uses native CoreAudio capture on macOS for `local-default` and `local-device` routes, while retaining the PulseAudio `parec` path for named PulseAudio routes such as remote Android sources. `caco stt daemon` reuses this live-capture path, so the ms-mac daemon no longer has to route the CoreAudio placeholder through `parec` after the patched binary is deployed.

## Operator-takeaway

The ms-mac failure was a real routing bug: CoreAudio `local-device` capture was going through the PulseAudio backend. This chunk fixes that code path and documents the contract; the remaining sgu24 symptom is separate rollout/config staleness on the phone rather than the same capture-backend bug.
