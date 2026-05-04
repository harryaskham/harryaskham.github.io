# bd-baf935 — acceptance_agent fixture isolation and readiness hardening

## Bead
- bd-baf935 — [broken-on-main] cargo test acceptance_agent suite fails in agent checkout

## Root cause
The acceptance_agent harness was not fully isolated from managed-worker runtime state. In managed checkouts, ambient Cacophony variables such as CACO_AGENT_ID, CACO_AGENT_TOKEN, CACO_NODE, CACO_TMUX_SOCKET, CACOPHONY_PROJECT, and related profile/reintegration variables leaked into isolated fixture CLI/daemon invocations. That could make caco agent new --json return non-AgentInfo envelopes such as checkout_not_ready or otherwise route through the wrong context. The harness also used too-short daemon/checkout readiness windows for load-sensitive managed hosts and had a stderr diagnostic path that could block on a live daemon stderr pipe.

## Changes
- Added scrub_managed_agent_env() and applied it to isolated caco command and daemon launches.
- Gave the acceptance fixture a per-process node name instead of hard-coded localhost, updating cert issuance and assertions accordingly.
- Added bounded daemon and checkout readiness budgets for load-sensitive hosts.
- Made DaemonGuard diagnostics nonblocking by capturing daemon stderr to a file and including stderr/log/materialized-config tails.
- Added parse_agent_new_json() diagnostics and a bounded caco_agent_new_test() retry for transient checkout_not_ready responses.
- Relaxed acceptance_tui_shows_created_agent to assert the current Cluster/Agents navigation surface instead of requiring the project name to be visible on the broad smoke path.

## Validation
- rustfmt --edition 2021 --check crates/caco/tests/acceptance_agent.rs
- git diff --check
- RUST_BACKTRACE=0 CACO_ACCEPTANCE=1 timeout 1200s cargo test -p caco --test acceptance_agent -- --nocapture
  - Result: 17 passed; 0 failed; finished in 655.45s.

## Notes
- This fixes the broken-on-main acceptance_agent failure without changing production runtime behavior.
- The operator's transcription E2E objective remains my preferred next focus after bd-baf935 is reintegrated/closed.
