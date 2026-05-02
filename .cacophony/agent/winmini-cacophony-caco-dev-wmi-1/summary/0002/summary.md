# Session summary — bd-8573ef status reachability under slow auth probe

## Goal

Fix the broken-on-main status reachability regression so `caco status --json` does not report `daemon.reachable=false` just because the authenticated `/api/v1/node` probe is slow, when the daemon is actually serving.

## Bead(s)

- `bd-8573ef` — [broken-on-main] integration_e2e status reachability false while authenticated /api/v1/node is slow

## Before state

- Failing tests: `cargo test -p caco --test integration_e2e status_probe_uses_bearer_token_for_reachability -- --test-threads=1` was the authoritative broken-on-main lane for this regression.
- Relevant metrics: `probe_daemon_api_port_serving(...)` in `crates/caco-cli/src/lib.rs` treated any authenticated probe transport failure as `daemon_reachable=false`. That meant a slow authenticated `/api/v1/node` could flip the entire status surface to "daemon down" even if the daemon was listening and an unauthenticated request would still get a quick 401.
- Context: the smallest contained fix was in the CLI status probe itself, not in the daemon. Status needed a bounded fallback path that preserved the authenticated probe first but stopped conflating slowness with unreachability.

## After state

- Failing tests: none in the focused caco-cli / integration_e2e lane.
- Relevant metrics: `probe_daemon_api_port_serving(...)` now treats success/401/403/503 as serving and, when an authenticated probe fails or times out, retries one fast unauthenticated `/api/v1/node` probe before declaring the daemon unreachable. A new caco-cli unit test pins that fallback behavior, and the exact integration_e2e regression test passes.
- Context: the status surface still prefers bearer-token probing, but it no longer misreports a live daemon as down solely because the authenticated node endpoint is slow.

## Diff summary

- Commits: `047450613`
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: `cargo test -p caco-cli status_reachability_falls_back_when_authenticated_node_probe_is_slow_bd_8573ef -- --nocapture`; `cargo test -p caco --test integration_e2e status_probe_uses_bearer_token_for_reachability -- --test-threads=1`; `cargo build -p caco`
- Behavioural delta: `caco status --json` keeps `daemon.reachable=true` for a live daemon when the bearer-token probe is merely slow, using a bounded unauthenticated 401-capable fallback instead of immediately declaring the daemon down.

## Operator-takeaway

This restores the important distinction between "authenticated node probe is slow" and "daemon is actually unreachable". The status surface now degrades more honestly under load instead of turning a slow authenticated check into a false outage.
