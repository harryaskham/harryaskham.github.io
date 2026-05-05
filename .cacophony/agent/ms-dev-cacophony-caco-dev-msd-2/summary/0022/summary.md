# bd-bc2c92 summary

Implemented cross-surface TTS solo/focus affordances on top of the bd-81805b CLI/runtime contract:

- TUI: `TtsDaemonLiveStatus` now carries temporary `solo`, `focus`, and `focus_duck_gain` fields populated from direct TTS daemon status and daemon proxy fallback. The Audio view and Speech popup daemon tab render active solo/focus separately from durable mute state.
- caco-web: workspace agent detail panes show a TTS attention control card with Solo agent, Focus agent, Solo project, Unsolo all, and Defocus all actions; the Speech pane shows current solo/focus state plus reset controls; agent list rows include quick Solo/Focus actions. Actions call the shared `/api/v1/tts/solo`, `/focus`, `/unsolo`, and `/defocus` endpoints.
- Android companion: `ConnectionManager` exposes TTS solo/focus/unsolo/defocus calls. `SpeechControlsScreen` shows temporary solo/focus state, exposes reset controls, and adds per-agent Solo/Focus actions in the existing per-agent audio list. Added a source-level Android unit test for the endpoints/actions.
- Documentation: SPEC/README note that operator UI surfaces mirror temporary attention state separately from mute policy and provide reset-to-normal controls.

Validation:

- `rustfmt --edition 2021 crates/caco-tui/src/app.rs crates/caco-tui/src/speech.rs crates/caco-tui/src/views/audio.rs crates/caco-tui/src/views/speech_popup.rs`
- `node --check crates/caco-web/static/workspace-panes.js`
- `node --check crates/caco-web/static/workspace-integrated.js`
- source assertion script across web/TUI/Android/README/SPEC
- Python brace-balance check for changed Android Kotlin files and new test
- `git diff --check`
- queued `cargo check -p caco-tui --lib`: `tj-f2e93568` passed (also earlier `tj-a2176946` passed)

Android Gradle validation notes:

- `tj-084edf73` failed immediately because this checkout does not include `companion/android/gradlew`.
- `tj-e683ccd5` failed immediately because ambient `gradle` is not on the queued worker PATH.
- No Android emulator/heavy build was attempted from this generic ms-dev worker. The source-level Kotlin checks above cover this slice; full Android builder validation can run from the configured Android surface if needed.
