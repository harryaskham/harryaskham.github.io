# Session summary — body-read cluster diagnostic recurrence

## Goal

Fix the post-`bd-f2c6b6` recurrence where `daemon-crash.log` still received routine nonfatal cluster diagnostics, now including body-read/pull-request failure wording alongside already-covered replication, discovery, handler-timeout, and TLS diagnostics.

## Bead(s)

- `bd-4f605b` — Cluster diagnostics still write to daemon-crash.log after bd-f2c6b6 close

## Before state

- Failing tests: none known for this bead.
- Relevant metrics: ms-mac log-monitor observed `daemon-crash.log` grow by 57,440 bytes during a healthy daemon/service window.
- Context: recent routing fixes covered many recurring cluster diagnostic shapes, but the bounded sweep still included pull request/body-read failures and mixed cluster diagnostics in the crash-log tail.

## After state

- Failing tests: none known for this bead.
- Relevant metrics: focused sidecar stderr-router tests now assert post-`bd-f2c6b6` body-read, pull-request, replication, materialized-message, queued-dispatch, handler-timeout, model-discovery, and TLS diagnostics route to `daemon.log`, not `daemon-crash.log`.
- Context: the classifier recognizes generic `body read failed` operational diagnostics in addition to the previously landed pull/body-read variants.

## Diff summary

- Commits: `da70e1e387`, `4fdf095d61`
- Files touched: `crates/caco-sidecar/src/lifecycle.rs`
- Tests: added `daemon_stderr_router_post_f2c6b6_cluster_diagnostics_bd_4f605b`.
- Behavioural delta: nonfatal pull-request/body-read diagnostics now stay out of `daemon-crash.log` with the broader cluster diagnostic set.
- Validation: `tj-189c1c97` passed `cargo test -p caco-sidecar bd_4f605b -- --nocapture`; `tj-d44604ab`, `tj-d921d207`, and `tj-95ccc22b` passed `cargo test -p caco-sidecar daemon_stderr_router -- --nocapture`, with `tj-95ccc22b` after the final rebase.

## Operator-takeaway

The latest recurrence was still operational cluster noise rather than crash evidence. The remaining body-read wording is now classified as diagnostics while unknown crash-shaped stderr remains conservative crash-log evidence.
