# bd-435011 — surface TTS daemon mute distinctly in TUI top bar

## Goal
Operator can tell at a glance whether the cluster's TTS daemon is
muted, without opening the speech popup.

## Bead(s)
- bd-435011 (P2 task). Title: "TTS daemon mute state should show in
  TUI top bar as well as local TUI state". No description body.

## Before state
- `crates/caco-tui/src/views/speech_indicator.rs` already shows
  daemon presence via 📡 (NORD14) when `should_show_daemon_icon()`
  is true, and ⚠📡 when daemon-running policy is active but the
  process is gone.
- The plain `📡` icon collapsed two distinct states: daemon
  up-and-active (will speak) and daemon up-but-muted at the daemon
  level (won't speak). Operators had to open the speech popup.
- Data was already plumbed: `tts_daemon_live_status.muted` is
  populated by the daemon status poller.

## After state
- New rendering inside the existing `should_show_daemon_icon`
  branch:
  - `📡✕` (NORD11 / red): daemon alive, daemon-side `muted=true`.
    Won't speak.
  - `📡`  (NORD14 / green): daemon alive, unmuted. Will speak.
- ⚠📡 (warning + dish, NORD13) and 🔊 / 🔇 paths unchanged.

## Diff summary
- `crates/caco-tui/src/views/speech_indicator.rs` (+48/-1):
  - In `speech_indicator_spans`, the `should_show_daemon_icon`
    arm now branches on `speech.tts_daemon_live_status.muted`.
  - Two new tests:
    - `shows_daemon_muted_icon_when_daemon_alive_and_muted` —
      with `tts_daemon_process_alive=true` and `live_status.muted=true`,
      rendered text contains both `📡` and `✕`.
    - `no_daemon_muted_marker_when_daemon_alive_and_unmuted` —
      with `muted=false`, `✕` is absent.

## Operator-takeaway
After binary roll, the top-bar speech indicator distinguishes
daemon-active-vs-daemon-muted at a glance. This pairs well with
bd-aa5299 (audio timeout bumped to 60s) — when troubleshooting
"why didn't anyone hear my speak", a single look at the top bar
will now tell you whether the daemon was muted or just busy.

## Tests
- `cargo build -p caco-tui` — clean.
- `cargo clippy -p caco-tui --all-targets -- -D warnings` — clean.
- `cargo test -p caco-tui --lib daemon_muted` — 2/2 pass.
