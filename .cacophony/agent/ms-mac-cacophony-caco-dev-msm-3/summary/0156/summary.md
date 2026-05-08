# Session summary — TTS spoken-name idle recovery retry

## Goal

Fix the reopened TTS spoken-name regression where the daemon had moved to the narrow `/api/v1/beads/all?limit=5000` lookup but could remain stuck in degraded backoff if no later feed event containing an unknown bead ID arrived to trigger another refresh attempt.

## Bead(s)

- `bd-4dbb61` — TTS spoken-name daemon_beads_all failures recur after bd-ee9408 close

## Before state

- Failing tests: none known in source for this slice.
- Relevant metrics: ms-mac log-monitor reported `daemon_beads_all` spoken-name failures at 16:05, 16:09, 16:20, and 16:35 UTC with retry state up to 900 seconds while daemon status, beads status, and sentinel reads were otherwise healthy.
- Context: `bd-ee9408` had replaced the heavyweight UI snapshot path and added success-side throttling, but the connected SSE loop still only retried on startup/reconnect or when a later bead-bearing event passed the backoff checks.

## After state

- Failing tests: none in focused validation.
- Relevant metrics: `RUST_MIN_STACK=33554432 cargo test -p caco-cli --lib tts_daemon_spoken_name -- --nocapture` passed 3/3 tests; `cargo clippy -p caco-cli --lib --no-deps -- -D warnings` passed.
- Context: the TTS daemon now wakes every 30 seconds while connected and otherwise idle, and only performs a spoken-name recovery lookup when a failure backoff exists and its cooldown has elapsed.

## Diff summary

- Code/content commits: `1294136db1`.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched: `crates/caco-cli/src/lib.rs`, `SPEC.md`, `.cacophony/agent/ms-mac-cacophony-caco-dev-msm-3/summary/pending/summary.md`.
- Tests: +1 unit test covering idle recovery-poll eligibility after failure cooldown.
- Behavioural delta: connected TTS daemons can recover from spoken-name lookup degradation without daemon/TTS restart and without requiring another feed event to contain an unknown bead ID, while preserving existing failure and success cooldown throttles.

## Operator-takeaway

The recurrence was not the old 60-second UI snapshot issue; it was a recovery-trigger gap. The TTS daemon now has a bounded idle retry path so a healthy daemon/beads endpoint can clear the degraded spoken-name state on its own.
