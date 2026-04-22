# bd-aa5299 — raise tts-daemon audio default timeout 25 → 60s, surface env knob in error

## Goal
Cut the chronic "speech request failed after 25s timeout window"
errors that flooded ms-mac's daemon log throughout 2026-04-22
(459 502 returns, 79 timeout records).

## Bead(s)
- bd-aa5299 (P2 bug). Filed by log-monitor after a full-day audit
  that surfaced the issue per-window dedup had been masking.

## Before state
- `DEFAULT_TTS_DAEMON_AUDIO_TIMEOUT_SECS = 25`. The constant lives
  in `crates/caco-cli/src/lib.rs:17294` and is read by
  `tts_daemon_audio_request_timeout()` (env override:
  `CACO_TTS_AUDIO_TIMEOUT_SECS`, fallback derived from
  `CACO_REQUEST_TIMEOUT_SECS - 5`).
- The synthesis provider's p99 latency clearly exceeds 25s in
  bursts; operator log says "first attempt timed out, retry
  succeeded" — i.e. the timeout was the actual failure mode,
  not the backend.
- The failure-summary log line included the timeout value but
  not the env-knob name, so operators reading dashboards had to
  trace into source to tune.

## After state
- `DEFAULT_TTS_DAEMON_AUDIO_TIMEOUT_SECS = 60`. Still well under
  the outer daemon request timeout (90s default), so playback
  errors still surface cleanly to `caco msg speak --wait`
  callers — no risk of stranding them behind the outer timeout.
- Failure-summary now reads:
  `speech request failed after 60s timeout window
   (override via CACO_TTS_AUDIO_TIMEOUT_SECS)`.

## Diff summary
- `crates/caco-cli/src/lib.rs` (+18/-3):
  - constant 25 → 60 with bd-aa5299 comment block explaining the
    rationale and the headroom under the outer request timeout.
  - one-liner change to the `Err(e)` arm of the daemon's
    `client.post(speak_url).send().await` to append
    `(override via CACO_TTS_AUDIO_TIMEOUT_SECS)`.
- Test
  `tts_daemon_audio_request_timeout_defaults_below_request_timeout`
  updated: now asserts `timeout.as_secs() == 60`.
  Companion test
  `tts_daemon_audio_request_timeout_respects_request_timeout_env`
  unchanged (CACO_REQUEST_TIMEOUT_SECS=18 → 13 derivation still
  holds, since 18-5=13 ≥ floor 5).

## Operator-takeaway
- After roll, expect a sharp drop in `/api/v1/audio/speech` 502
  count. If the synthesis backend continues to spike past 60s
  on ms-mac (or has different characteristics on a slower host),
  bump per-host with
  `export CACO_TTS_AUDIO_TIMEOUT_SECS=120` — guidance is now
  visible in the error message itself.
- Followup possible (not in scope here): add a single retry on
  `Err(timeout)` inside the playback loop. The change touches a
  deeply-nested async closure and warrants its own bead.

## Tests
- `cargo build -p caco-cli` — clean.
- `cargo clippy -p caco-cli --all-targets -- -D warnings` — clean.
- `cargo test -p caco-cli --lib tts_daemon_audio_request_timeout`
  — 2/2 pass.
