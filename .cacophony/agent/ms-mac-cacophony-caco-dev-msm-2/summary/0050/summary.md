# Session summary — transient launcher PATH retry for persistent agents

## Goal

Implement `bd-b1c944` so persistent auto-start agents that fail during a transient launcher/PATH outage for `git` do not remain buried under ordinary exponential crash backoff. The intended operator-facing behavior is a clearly classified, retryable startup-tool failure that converges once the launcher PATH is healthy again.

## Bead(s)

- `bd-b1c944` — Persistent auto-start agents remain failed after transient launcher PATH loses git

## Before state

- Failing tests: none known for this bead at start.
- Relevant metrics: ms-mac outage examples showed persistent declarations such as update-helper failing with messages like `git config user.name ... failed: error: tool git not found` / `git clone --shared ... failed: error: tool git not found`, then remaining failed after launcher/PATH recovery.
- Context: `bd-a01a3c` had just landed; controller assigned this single bead and explicitly said not to do release, runner restart, broad ms-mac repair, or extra claims.

## After state

- Failing tests: none observed in targeted validation.
- Relevant metrics: queued `CARGO_BUILD_JOBS=2 cargo test -p caco-daemon --lib bd_b1c944 -- --test-threads=2` passed (`tj-48be139c`); queued `CARGO_BUILD_JOBS=2 cargo check -p caco-daemon --lib` passed (`tj-7187ed12`); `git diff --check` passed locally.
- Context: daemon persistent launch now recognizes `tool git not found` as a transient startup-tool/launcher-PATH class, records a concise remediation, uses a fixed short retry delay, and reports structured warnings with `transient-startup-tool` / `launcher-path` labels instead of a generic launch crash error.

## Diff summary

- Commits: `a72302e05`
- Files touched: `crates/caco-daemon/src/persistent.rs`, `crates/caco-daemon/src/lib.rs`
- Tests: added one focused unit test for `bd-b1c944` covering detection, formatted remediation, fixed retry delay, and retry-after expiry behavior.
- Behavioural delta: persistent launch failures containing `tool git not found` now call `mark_transient_startup_tool_failure`, preserving visibility while resetting crash-count backoff and retrying after `TRANSIENT_STARTUP_TOOL_RETRY_SECS` rather than escalating into long exponential restart delay.

## Operator-takeaway

A transient missing-`git` launcher PATH should now heal on the next short persistent reconcile retry after PATH recovery, while still leaving a clear warning explaining the launcher PATH remediation if it persists.
