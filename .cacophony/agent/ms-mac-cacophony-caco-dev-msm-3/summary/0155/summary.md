# Session summary — TTS spoken-name refresh success cooldown

## Goal

Continue the reopened `bd-ee9408` investigation after live ms-mac refreshed to the beads-all implementation but still produced repeated `daemon_beads_all` lookup failures. The goal was to reduce avoidable on-demand refresh pressure after a successful cache fill, especially during feed bursts containing bead IDs absent from the current cache.

## Bead(s)

- `bd-ee9408` — TTS spoken-name refresh times out daemon UI snapshot on ms-mac

## Before state

- Failing tests: none in source; runtime evidence showed repeated TTS spoken-name lookup failures.
- Relevant metrics: log-monitor appended daemon-beads-all failures at 22:50, 22:57, 23:03, 23:08, recovery at 23:17, recurrence/recovery at 23:19/23:21, then another failure at 23:21:26. Live status was on `/api/v1/beads/all?limit=5000` but could refresh again seconds after a successful fill when a feed message referenced an uncached bead ID.
- Context: the original 60s UI snapshot timeout path was replaced, but on-demand refresh still had no success-side throttle.

## After state

- Failing tests: none in focused queued validation.
- Relevant metrics: queued `tj-3a58aceb` passed `tts_daemon_spoken_name_refresh_backs_off_after_failure`; queued `tj-3b37a8e5` passed `tts_status_serializes_runtime_mute_source_bd_fe2d81`; queued `tj-b09dfd96` passed `cargo clippy -p caco-cli --lib --no-deps -- -D warnings`.
- Context: TTS now records the last successful spoken-name refresh instant and suppresses immediate on-demand refresh retries for 60 seconds after success, while preserving failure backoff behavior.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched: `crates/caco-cli/src/lib.rs`, `SPEC.md`, `.cacophony/agent/ms-mac-cacophony-caco-dev-msm-3/summary/pending/summary.md`.
- Tests: +0 net tests; extended the existing refresh-backoff unit test to cover the success cooldown.
- Behavioural delta: a successful spoken-name cache refresh creates a short quiet period before on-demand unknown-bead refreshes can hit the daemon again, reducing feed-burst amplification against `/api/v1/beads/all?limit=5000`.

## Operator-takeaway

The live refresh proved the endpoint migration worked, but repeated unknown-bead on-demand lookups could still hammer the narrower beads endpoint immediately after recovery. This patch adds a success-side cooldown so recovery stays quiet instead of bouncing back into fresh lookup failures seconds later.
