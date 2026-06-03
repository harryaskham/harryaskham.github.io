# Session summary — bd-d9d11e bound/reap stuck paplay playback clients (silent-TTS fix)

## Goal

Fix a P2 TTS bug where helsinki stopped emitting audible speech even though
the TTS daemon reported healthy (muted=false, queue~0, pulse=connected). Root
cause was at the playback layer: when a remote pulse sink stops draining
mid-stream, `paplay` blocks forever after EOF and never exits, so orphaned
playback clients accumulate (helsinki had 5, ages 22h–1d19h) and clog the path
so new speech is inaudible. Make the playback path self-healing by bounding and
reaping hung playback children. While validating, also clear a pre-existing
broken-on-main caco-tui clippy failure that would otherwise block the gate.

## Bead(s)

- `bd-d9d11e` — TTS daemon should bound/reap stuck paplay playback clients
  (hung paplay clogged remote sink -> silent TTS) [P2 bug]
- `bd-ed4808` — [broken-on-main] caco-tui clippy needless_borrow on app.rs
  ref click_ranges (2 sites) [P1 bug, filed + fixed this session]

## Before state

- Failing tests: pre-existing broken-on-main — `cargo clippy -p caco-tui --lib
  -- -D warnings` failed with 2 `clippy::needless_borrow` errors at
  app.rs:12900 and :14071 (`if let Some(ref click_ranges) = ...get(&tile_id)`),
  present on clean origin/main, unrelated to the TTS work.
- `play_wav_pulse` post-write child-wait loop polled `try_wait()` with a 10ms
  sleep but had NO wall-clock upper bound, so a `paplay` blocked on a stalled
  remote sink spun there indefinitely and the child never exited.

## After state

- Failing tests: none. caco-tui playback suite 44/44 green (incl. 4 new
  deadline tests); `cargo clippy -p caco-tui --lib -- -D warnings` exits 0.
- `play_wav_pulse` now bounds the child-wait loop by a wall-clock deadline; on
  timeout it kills+reaps the child and returns a distinct
  `Failed("pulse playback timed out ... remote sink may be stalled ...")`.
- The 2 pre-existing clippy::needless_borrow sites are fixed (ref removed).

## Diff summary

- Code commits: `5cacaa3f2` (bd-ed4808 clippy fix), `8885ba6a2` (bd-d9d11e TTS
  reap). Final landed squash SHA(s) come from the reintegration receipt.
- Summary artefact commit: intentionally omitted (self-reference).
- Files touched: `crates/caco-tui/src/playback.rs` (+118: deadline consts +
  pure `pulse_playback_deadline` helper + wait-loop deadline wiring + 4 tests),
  `crates/caco-tui/src/app.rs` (2-line ref removal at the 2 clippy sites).
- Tests: +4 (`pulse_playback_deadline_*`: floor, unparseable, margin, ceiling).
- Behavioural delta: a stalled remote pulse sink can no longer hang a `paplay`
  client forever; hung players are reaped after clip_duration + 15s margin
  (clamped to [20s, 300s]), preventing the silent-TTS accumulation. No change
  to the healthy fast-path (children that exit normally are unaffected).

## Operator-takeaway

The silent-TTS-on-helsinki failure mode (daemon reports healthy, but no audio)
is now self-healing at the source: playback children get a bounded wall-clock
budget and are killed+reaped on timeout with a diagnosable
`pulse playback timed out (remote sink may be stalled ...)` outcome, instead of
piling up orphaned `paplay` clients that clog the path. Future hardening the
bead suggests but this slice did NOT implement: surfacing a stuck-playback-
client count in `caco tts status` / `tts io output show`, and detecting
remote-sink drain stalls as an explicit degraded status. Those are additive
follow-ups; this slice fixes the core hang. The reap deadline lives in the
shared `caco-tui::playback` module because the TTS daemon plays through it.
