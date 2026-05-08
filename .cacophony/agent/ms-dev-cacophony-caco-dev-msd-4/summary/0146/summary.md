# Session summary — UI snapshot/profile discovery stderr recurrence

## Goal

Fix the post-`bd-dc1fda` recurrence where `daemon-crash.log` still received routine nonfatal diagnostics, now from UI snapshot timeout/fetch paths and profile discovery after checkout updates.

## Bead(s)

- `bd-b7775a` — Cluster diagnostics still write to daemon-crash.log after bd-dc1fda close

## Before state

- Failing tests: none known for this bead.
- Relevant metrics: ms-mac log-monitor observed `daemon-crash.log` grow by 58,704 bytes during a healthy daemon/service window.
- Context: prior log-routing beads covered replication summaries, queued dispatches, TLS peer alerts, idle advisories, and checkout refresh/index-lock diagnostics. The latest bounded sweep still showed non-crash UI snapshot handler timeout/profile discovery text reaching the crash log.

## After state

- Failing tests: none known for this bead.
- Relevant metrics: focused sidecar stderr-router tests now assert UI snapshot timeout/read/fetch diagnostics and profile discovery notices route to `daemon.log`, not `daemon-crash.log`.
- Context: the classifier recognizes the latest post-`bd-dc1fda` diagnostic shapes while preserving conservative crash routing for unknown stderr.

## Diff summary

- Commits: `f9937ccc9d`, `6a0233a7ab`
- Files touched: `crates/caco-sidecar/src/lifecycle.rs`
- Tests: added `daemon_stderr_router_ui_snapshot_profile_discovery_are_diagnostics_bd_b7775a`.
- Behavioural delta: nonfatal UI snapshot and profile-discovery diagnostics now stay out of `daemon-crash.log`.
- Validation: `tj-fede09c3` passed `cargo test -p caco-sidecar bd_b7775a -- --nocapture`; `tj-7bbf4039` passed `cargo test -p caco-sidecar daemon_stderr_router -- --nocapture`; `tj-ba9aaf7a` and `tj-e511a600` passed the same focused subset after rebase/stale-branch recovery. Earlier `tj-a7d081f8` failed because the first regression guard missed the exact `profile(s) discovered` wording; the classifier and test were corrected before the passing runs.

## Operator-takeaway

The new recurrence was another routine diagnostics shape, not a daemon crash. UI snapshot timeout/fetch noise and profile discovery notices now follow the same non-crash routing as the earlier replication and checkout-refresh fixes.
