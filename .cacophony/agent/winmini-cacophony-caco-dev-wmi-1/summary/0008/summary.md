# Session summary — bd-ee6e5f TTS stale-skip health reporting

## Goal

Make the Helsinki TTS stale-skip problem diagnosable without weakening the stale-age safety filter. The slice for this session was to surface enough operator-visible status and log context to tell the difference between expected reconnect backfill and sustained TTS lag.

## Bead(s)

- `bd-ee6e5f` — Helsinki TTS daemon skips many speak events as stale after falling more than five minutes behind

## Before state

- Failing tests: none known for this code path, but the operator only had raw log lines like `TTS skipped stale speak message ... age=366s threshold=300s` to work from.
- Relevant metrics: the TTS daemon already skipped stale feed events after `max_age_before_skip_secs` (default 300s), but it did not track stale-skip counts, last/max stale age, reconnect context, or feed-connect time in `caco tts status` / audio-health views.
- Context: on Helsinki this made bursts of stale skips look like unexplained silent playback loss. Operators could see the skip lines, but not whether they were likely reconnect backfill or a daemon that had stayed connected while falling badly behind.

## After state

- Failing tests: none in the targeted lane used for this bead.
- Relevant metrics: the TTS daemon now tracks `total_stale_skips`, `last_stale_skip_at`, `last_stale_skip_age_secs`, `max_stale_skip_age_secs`, `last_stale_skip_context`, and `last_feed_connected_at`, and includes reconnect-context detail in stale-skip trace/log lines.
- Context: `caco tts status` and the broader audio-health rendering now expose stale-skip health directly, and each stale skip is labeled as either `after-feed-connect` or `while-connected` so reconnect backfill can be distinguished from steady-state lag without changing playback safety.

## Diff summary

- Commits: `c75c79ff3`
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: initial queued targeted test run `tj-8008441d` failed with `No space left on device`; recovered via safe local `cargo clean` only. Then validated with `cargo fmt --check crates/caco-cli/src/lib.rs` (via `rustfmt --edition 2021 --check crates/caco-cli/src/lib.rs`), queued compile-only `caco build run --wait true --command 'cargo check -p caco-cli --tests'` (`bj-3d9505fa`), and queued focused tests `caco test run --wait true --command 'cargo test -p caco-cli tts_status_serializes_runtime_mute_source_bd_fe2d81 -- --nocapture && cargo test -p caco-cli classify_tts_stale_skip_context_distinguishes_reconnect_backfill_bd_ee6e5f -- --nocapture && cargo test -p caco-cli extract_speakable_authored_at_prefers_inner_payload_ts -- --nocapture'` (`tj-565066f8`).
- Behavioural delta: stale events are still skipped and acked exactly as before, but operators now get durable counters plus reconnect-context classification in status/logs instead of only per-message skip lines.

## Operator-takeaway

This fix does not make old queued narration play; it makes stale-skip bursts explain themselves. If Helsinki shows stale skips immediately `after-feed-connect`, that points to reconnect backlog. If they show up `while-connected`, the node is likely falling behind while its stream stays up and needs deeper performance or reliability investigation.
