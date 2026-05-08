# Session summary — checkout refresh stderr recurrence

## Goal

Fix the post-`bd-7c8172` recurrence where `daemon-crash.log` still received routine nonfatal cluster diagnostics, now including checkout-refresh and Git `index.lock` diagnostic text.

## Bead(s)

- `bd-dc1fda` — Cluster diagnostics still write to daemon-crash.log after bd-7c8172 close

## Before state

- Failing tests: none known for this bead.
- Relevant metrics: ms-mac log-monitor observed `daemon-crash.log` grow by 58,288 bytes during a healthy daemon/service window.
- Context: the crash-log tail still contained routine replication/cluster diagnostics plus checkout refresh `index.lock` text. Previous beads covered several replication, queued-dispatch, TLS, and idle-advisory shapes, but checkout-refresh/index-lock diagnostics were still conservative unknown stderr.

## After state

- Failing tests: none known for this bead.
- Relevant metrics: focused sidecar stderr-router tests now assert checkout refresh errors, stale Git lock cleanup summaries, and blocked `.git/index.lock` diagnostics route to `daemon.log`, not `daemon-crash.log`.
- Context: the daemon stderr classifier recognizes checkout refresh/index-lock operational diagnostics as non-crash lines while preserving conservative crash routing for unknown stderr.

## Diff summary

- Commits: `be964d1b3d`, `6c99fdfdd9`
- Files touched: `crates/caco-sidecar/src/lifecycle.rs`
- Tests: added `daemon_stderr_router_checkout_refresh_index_lock_is_diagnostic_bd_dc1fda`.
- Behavioural delta: nonfatal checkout refresh and Git index-lock cleanup diagnostics now stay out of `daemon-crash.log`.
- Validation: `tj-cef655cc` passed `cargo test -p caco-sidecar bd_dc1fda -- --nocapture`; `tj-bda5e564` passed `cargo test -p caco-sidecar daemon_stderr_router -- --nocapture`; `tj-eedf0cd2` passed the same focused subset after resolving the rebase conflict with upstream replication-summary coverage.

## Operator-takeaway

The newest recurrence was a checkout-refresh/index-lock diagnostic shape, not a daemon crash. That operational text now routes with daemon diagnostics instead of inflating crash evidence.
