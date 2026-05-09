# Session summary — Legacy idle-advisory diagnostic guard

## Goal

Stop legacy structured persistent-idle advisory diagnostics from continuing to populate `daemon-crash.log` after the newer text-shape guard in `bd-1113b7`, especially when old `bd-db327a` advisory lines are replayed inside startup previous-stderr banners.

## Bead(s)

- `bd-5d628b` — caco-aks idle advisory still writes to daemon-crash.log after bd-1113b7 close

## Before state

- Failing tests: none known before this slice.
- Relevant metrics: log-monitor reported `daemon-crash.log` at 1,152,074 bytes with 44 timestamped lines in the 2026-05-09T02:21:29Z to 2026-05-09T02:36:29Z sweep; 17 were idle-advisory lines.
- Context: `bd-1113b7` suppressed modern `persistent agent ... advisory only; live runtime not auto-restarted` text, but the crash-log tail also contained legacy structured lines like `bd-db327a: persistent idle advisory ... idle_secs=... threshold_secs=... no auto-restart without positive death evidence`.

## After state

- Failing tests: none from focused validation.
- Relevant metrics: queued validation passed after rebase: `tj-9266f195` for the focused daemon idle-advisory stderr-mirror test and `bj-82c3f9db` for `cargo check -p caco-daemon --lib`.
- Context: the stderr mirror guard now recognizes both modern natural-language idle advisories and legacy structured persistent idle advisory diagnostics, including startup previous-stderr replay wrappers.

## Diff summary

- Commits: `c8e023156a`.
- Files touched: `crates/caco-daemon/src/logging.rs`.
- Tests: extended 1 daemon unit test / -0 / flipped 0.
- Behavioural delta: warning-only persistent-idle diagnostics in either modern or legacy shape stay out of daemon stderr/crash-log while ordinary warnings and panic-shaped output still mirror.

## Operator-takeaway

The post-`bd-1113b7` recurrence was a second idle-advisory text shape, not new crash evidence. The guard now covers both the modern and legacy diagnostic formats so old tails decay instead of being re-emitted.
