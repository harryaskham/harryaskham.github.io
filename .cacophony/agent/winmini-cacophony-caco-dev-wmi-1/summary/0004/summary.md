# Session summary — bd-2c7944 summaries next-index slow daemon tolerance

## Goal

Fix the recorded-summary allocation bug where `caco summaries next-index` could return `0000` for an agent that already had recorded summaries, simply because its daemon-authoritative summaries lookup timed out much earlier than the real summaries list path.

## Bead(s)

- `bd-2c7944` — caco summaries next-index returns 0000 when summaries list works but daemon lookup exceeds 3s
- discovered during collab-mode after the ready queue drained post-`bd-24b557`

## Before state

- Failing tests: no existing targeted regression pinned this timeout mismatch.
- Relevant metrics: on winmini, `caco summaries list --agent $CACO_AGENT_ID --limit 1 --json` succeeded after about 14.7 seconds and showed `reintegration_index: 3`, while `caco summaries next-index --agent $CACO_AGENT_ID --json` returned after about 3.7 seconds with `daemon_status: "daemon_unreachable"`, `daemon_max: null`, and `next_index: 0`.
- Context: `crates/caco-cli/src/summary_cmd.rs::fetch_daemon_max_summary_index()` used a bespoke blocking reqwest client with a hardcoded 3-second total timeout, so the authoritative summaries probe could fail even when the same daemon endpoint was merely slow rather than unreachable.

## After state

- Failing tests: none in the focused caco-cli summary lane.
- Relevant metrics: the daemon-authoritative `next-index` probe now uses a 20-second bounded timeout with a 1-second connect timeout, and a new regression test proves a 4-second delayed but healthy summaries endpoint still returns the existing max reintegration index instead of degrading to `daemon_unreachable`.
- Context: `caco summaries next-index` still falls back safely when the daemon is genuinely unavailable, but it no longer treats an ordinarily slow summaries API as if no recorded summaries exist.

## Diff summary

- Commits: `ba9add314`
- Files touched: `crates/caco-cli/src/summary_cmd.rs`
- Tests: `cargo test -p caco-cli fetch_daemon_max_summary_index_tolerates_slow_successful_daemon_bd_2c7944 -- --nocapture`; `cargo test -p caco-cli next_summary_index_consults_daemon_max_bd_7436de -- --nocapture`; `cargo test -p caco-cli summaries_next_index_accepts_agent_flag_bd_3186f3 -- --nocapture`
- Behavioural delta: `caco summaries next-index` now gives the daemon-authoritative summaries lookup enough time to succeed on loaded hosts, preventing false `0000` allocations when summaries already exist for the agent.

## Operator-takeaway

This protects a sharp recorded-summary footgun: a slow summaries daemon should no longer trick `next-index` into reusing index `0000` or otherwise colliding with existing recorded summaries. The command now better matches the real latency of the summaries surface it depends on.
