# bd-81805b summary

Implemented CLI-first temporary TTS attention controls:

- Added `caco tts solo --agent <id>` and `caco tts solo --project <name>` to suppress non-matching speech without writing scoped mute/config policy.
- Added `caco tts unsolo [--all]` and top-level `caco unsolo [--all]` to clear solo state, with `--all` clearing focus too.
- Added `caco tts focus --agent <id>` to keep the focused agent at full volume while ducking non-focused agents to 20% and dispatching playback in non-blocking focus mode.
- Added `caco tts defocus [--all]` and top-level `caco defocus [--all]` to clear focus state, with `--all` clearing solo too.
- Exposed solo/focus state in TTS daemon status and `tts status --explain`; `list-mutes` reports the temporary overlay separately from config/runtime mute rules.
- Added TTS daemon control endpoints and main-daemon proxy routes for solo, unsolo, focus, and defocus.
- Added playback gain support for focused ducking, plus tests for PCM16 WAV ducking/clamping.
- Updated SPEC, README, and AGENTS TTS policy docs.
- Filed/linked follow-up UI surface parity bead `bd-bc2c92` with explicit dependency on this CLI/runtime contract.

Validation:

- `git diff --check`
- `rustfmt --edition 2021 crates/caco-cli/src/lib.rs crates/caco-tui/src/playback.rs crates/caco-daemon/src/lib.rs`
- `caco test run --wait --command "cargo check -p caco-cli -p caco-tui --lib" --cwd "$PWD"` => `tj-c41f4988` passed
- `caco test run --wait --command "cargo check -p caco-daemon --lib" --cwd "$PWD"` => `tj-0adb1a03` passed
- `caco test run --wait --command "cargo test -p caco-tui apply_gain_to_pcm16_wav -- --test-threads=1" --cwd "$PWD"` => `tj-80f44017` passed
- `caco test run --wait --command "cargo test -p caco-cli tts_attention_scope -- --test-threads=1" --cwd "$PWD"` => `tj-3ac7a391` passed
- `caco test run --wait --command "cargo test -p caco-cli tts_solo_scope -- --test-threads=1" --cwd "$PWD"` => `tj-3befeaf0` passed

Notes:

- Earlier queue attempts `tj-e30bb92a`, `tj-acb46998`, and `tj-cd4fab57` were daemon-restart-recovered infrastructure outcomes, not code test failures; equivalent retries passed.
