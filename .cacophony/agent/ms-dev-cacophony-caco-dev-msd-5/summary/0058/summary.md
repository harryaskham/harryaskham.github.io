# Session summary — TTS spoken-name post-438fa6 diagnostics

## Goal

Handle the recurrence where the TTS daemon still logged a `daemon_beads_all` spoken-name lookup failure after `bd-438fa6` closed. The current mainline code already enriches this path with source, endpoint, transport class, error chain, and local daemon probe details, so this session pinned the exact on-demand failure log shape as regression coverage.

## Bead(s)

- `bd-95cfeb` — TTS daemon_beads_all failures recur after bd-438fa6 close

## Before state

- The log monitor reported a post-`bd-438fa6` on-demand spoken-name refresh failure showing only the top-level reqwest error in live daemon logs.
- Current code from `bd-438fa6` already adds a bounded `/api/v1/node` local daemon probe to the `daemon_beads_all` failure detail and preserves recovery backoff.

## After state

- Added a focused caco-cli regression that constructs the on-demand failure log line from a closed local port and asserts it includes `source=daemon_beads_all`, `/api/v1/beads/all?limit=5000`, transport `class=`, `error_chain=[...]`, `daemon_probe=...`, and `local daemon probe failed`.
- This verifies the current code logs a bounded degraded reason rather than the unhelpful top-level request error from the observed stale/live recurrence.
- Focused validation passed: `tj-419aafff` ran `RUST_MIN_STACK=33554432 cargo test -p caco-cli --lib tts_daemon_spoken_name_on_demand_failure_log_is_diagnostic_bd_95cfeb -- --nocapture` successfully.

## Diff summary

- Commits: current branch commit for `bd-95cfeb`; final landed squash SHA will be in the reintegration receipt.
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: +1 caco-cli TTS spoken-name diagnostic regression.
- Behavioural delta: no new runtime branch was required beyond the already-landed diagnostic enrichment; the observed post-close recurrence is now pinned to prevent regression.

## Operator-takeaway

Current code should log actionable TTS spoken-name failure diagnostics for this path. If live logs still show the older terse form, the remaining issue is likely rollout/runtime convergence rather than missing diagnostic logic.
