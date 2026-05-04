# Session summary — TTS spoken-name snapshot recovery diagnostics

## Goal

Make the headless TTS daemon’s bead spoken-name snapshot refresh recover cleanly after transient 503/startup-window failures and expose enough operator-facing metadata to distinguish harmless startup races from persistent broken snapshot routing.

## Bead(s)

- `bd-fa8a97` — Make TTS bead spoken-name snapshot refresh recover cleanly after 503s

## Before state

- Failing tests: none known at session start.
- Relevant metrics: operator logs showed `bead spoken-name snapshot refresh failed (retry in 60s): snapshot request returned HTTP 503 Service Unavailable` while the board later reported strict-green.
- Context: the TTS daemon already had bounded backoff and quiet subsequent retries, but the failure text was opaque and `caco tts status` / `status --explain` did not expose the snapshot refresh endpoint, cached-count, last HTTP/upstream status, last error, or retry metadata.

## After state

- Failing tests: none observed.
- Relevant metrics: final `docs/validate-pages.sh` passed; final `git diff --check` passed; queued `tj-c232fccf` passed `RUST_MIN_STACK=33554432 cargo test -p caco-cli tts_daemon_spoken_name_snapshot_reports_503_metadata_then_recovers --lib -- --test-threads=2`; queued `tj-4e6036b0` passed `RUST_MIN_STACK=33554432 cargo test -p caco-cli tts_status_serializes_runtime_mute_source_bd_fe2d81 --lib -- --test-threads=2`.
- Context: snapshot refresh failures now carry source/endpoint/detail/HTTP/upstream metadata, runtime status tracks cached names, consecutive failures, last success/failure, and next retry, and a successful refresh after a degraded period logs an INFO recovery confirmation.

## Diff summary

- Commits: `d43d5dd66`.
- Files touched: `crates/caco-cli/src/lib.rs`, `README.md`, `SPEC.md`, `AGENTS.md`, `docs/cli.html`.
- Tests: +1 async focused regression for HTTP 503 then recovery; existing status serialization coverage updated for `spoken_name_snapshot`.
- Behavioural delta: `caco tts status` and `caco tts status --explain` can now show bead spoken-name snapshot health, while the daemon records richer failure metadata and confirms recovery instead of leaving operators with repeated opaque 503 warnings.

## Operator-takeaway

The next time the TTS daemon hits a transient bead snapshot 503 during startup or maintenance, operators should see whether the cache recovered, exactly which endpoint/source failed, and whether a persistent routing problem remains.
