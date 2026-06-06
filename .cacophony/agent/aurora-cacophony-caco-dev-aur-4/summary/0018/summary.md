# Session summary — bd-d35861 agent audio controls

## Goal

Expose first-party per-agent audio attention controls in the main agent-detail operator surfaces, alongside the existing heartbeat override controls. The goal was to make mute/unmute, solo/unsolo, and focus/unfocus visible and clickable without replacing the daemon-owned TTS and heartbeat APIs.

## Bead(s)

- `bd-d35861` — Add mute/unmute, solo/unsolo, focus/unfocus controls to all agent surfaces

## Before state

- TUI Agent Detail had `HB On` / `HB Off` text buttons and no per-agent TTS mute, solo, or focus buttons in the action row.
- caco-web Agent Detail had a separate Heartbeat section with text Enable/Disable buttons and no per-agent TTS attention controls.
- SPEC required heartbeat controls in Agent Detail but did not explicitly state the new audio attention control contract.
- Mobile/native companion surfaces already had active specialist work and existing wrist/audio controls; this slice avoided overlapping those active lanes and focused on the TUI + caco-web operator surfaces.

## After state

- TUI Agent Detail action rows prepend compact icon-only TTS controls for daemon mute/unmute, solo/unsolo, and focus/unfocus when TTS daemon live status is reachable.
- TUI heartbeat controls now render as icon-only heart affordances, with no visible `H` shortcut, while continuing to call the first-party heartbeat API.
- caco-web Agent Detail now renders matching audio attention buttons before heartbeat icons and routes them through existing `/api/v1/tts/*` endpoints.
- SPEC now records the Agent Detail audio-attention control contract and the heartbeat icon-state expectation.

## Diff summary

- Code/content commits: `3f8d794c7a` (`bd-d35861: add TUI agent audio controls`), `aae6d53a8e` (`bd-d35861: add web agent audio controls`), `e72a6340c5` (`bd-d35861: document agent audio controls`); final landed squash SHA will come from the reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched: `crates/caco-tui/src/app.rs`, `crates/caco-tui/src/speech.rs`, `crates/caco-tui/src/views/agent_detail.rs`, `crates/caco-tui/src/views/button.rs`, `crates/caco-web/static/app.js`, `crates/caco-web/src/tests.rs`, `SPEC.md`, `.cacophony/agent/aurora-cacophony-caco-dev-aur-4/summary/pending/summary.md`.
- Tests: added focused TUI button/Agent Detail assertions and caco-web static JS assertions for the new control wiring.
- Validation:
  - `tj-9893b685` passed: `RUST_MIN_STACK=33554432 cargo test -p caco-tui bd_d35861 -- --nocapture`
  - `tj-a281123a` passed: focused `tts_control_buttons_reflect_mute_solo_focus_state_bd_d35861`
  - `tj-8a6252f2` passed: focused `bd_d35861` button tests before the Agent Detail guard was added
  - `tj-0cc58974` passed: `RUST_MIN_STACK=33554432 cargo test -p caco-tui views::button::tests:: -- --test-threads=1`
  - `tj-c1db0ebe` passed: `cargo clippy -p caco-tui -- -D warnings`
  - `node --check crates/caco-web/static/app.js` passed locally
  - `tj-a1f33e19` passed: `cargo test -p caco-web app_js_agent_detail_audio_and_heartbeat_controls_bd_d35861 -- --nocapture`
- Behavioural delta: operators can see and click per-agent TTS mute/solo/focus controls in TUI and caco-web Agent Detail, while heartbeat remains daemon-backed and is represented by icon-state buttons.

## Operator-takeaway

This lands the shared TUI/web foundation for per-agent audio attention controls without inventing new protocols: every new button uses existing first-party TTS daemon or heartbeat endpoints. Mobile/native companion parity should continue in their specialist lanes where those surfaces are already under active polish.
