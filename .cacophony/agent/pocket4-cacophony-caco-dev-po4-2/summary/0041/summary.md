# Session summary — agent creation now has a dedicated longer request timeout for slow Pi startup

## Goal

Return to the long-claimed P1 bug `bd-8a7c6b` after the profile-audit cleanup and make `caco pi` / managed runtime shorthand resilient to slow startup timeouts. The initial question was whether the fix belonged in CLI recovery, daemon request-timeout policy, or both.

## Bead(s)

- `bd-8a7c6b` — Make caco pi runtime shorthand resilient to slow startup timeouts

## Before state

- `dispatch_runtime_shorthand(...)` in `crates/caco-cli/src/lib.rs` still just calls `dispatch_agent_new(...)` and then attaches; there was no special slow-start recovery in the shorthand wrapper.
- The daemon’s request-timeout middleware in `crates/caco-daemon/src/lib.rs` had dedicated longer budgets for:
  - `/api/v1/audio/speech`
  - `/api/v1/ui/snapshot`
  - `/api/v1/summaries...`
- But `POST /api/v1/projects/<project>/agents` still used the generic 30s request timeout.
- Pi startup can legitimately exceed that generic budget on loaded hosts because checkout/bootstrap/profile resolution, tmux session creation, readiness sentinel writes, and runtime launch verification all happen before the create endpoint returns.

## After state

- Added a dedicated server-side timeout budget for agent creation:
  - `DEFAULT_AGENT_CREATE_REQUEST_TIMEOUT_SECS: u64 = 90`
- Added a dedicated env override:
  - `CACO_AGENT_CREATE_REQUEST_TIMEOUT_SECS`
- Added path detection:
  - `is_agent_create_path(path: &str)`
- Updated `request_timeout_for_path(...)` so `/api/v1/projects/<project>/agents` now uses the longer agent-create timeout instead of the generic 30s budget.
- Added a targeted daemon test:
  - `agent_create_path_gets_extended_request_timeout_bd_8a7c6b`

## Diff summary

- Files touched:
  - `crates/caco-daemon/src/lib.rs`
- Validation (queued, first-party):
  - `caco test run --wait true --command "cargo test -p caco-daemon agent_create_path_gets_extended_request_timeout_bd_8a7c6b -- --nocapture" --cwd "$PWD"`
  - `caco build run --wait true --command "cargo build -p caco-daemon" --cwd "$PWD"`
- Behavioural delta:
  - slow Pi startup no longer has to fit inside the generic 30s daemon request timeout for the create endpoint
  - operators can tune the create budget independently via `CACO_AGENT_CREATE_REQUEST_TIMEOUT_SECS`

## Operator-takeaway

This is the minimal first cut for the reported `caco pi` timeout: instead of teaching the shorthand wrapper a bespoke recovery ritual first, the daemon now gives agent creation a timeout budget that matches real Pi startup costs. If later evidence shows even this is insufficient or that timeout recovery still needs better CLI UX, that can be layered on top — but the most direct 30s budget mismatch is now fixed at the authoritative server path.
