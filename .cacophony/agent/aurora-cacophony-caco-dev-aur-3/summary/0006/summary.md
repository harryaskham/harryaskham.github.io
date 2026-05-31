# Session summary — background non-persistent agent resume (bd-1cfc19)

## Goal

Convert `handle_agent_resume`'s slow path to the accept-and-background pattern so
a slow Pi resume no longer transport-times-out at the 120s lifecycle timeout,
completing the lifecycle-async follow-up split off from bd-14114e. This was
flagged the highest-risk of the conversions, so the work mirrors the already
landed refresh/recreate handlers exactly.

## Bead(s)

- `bd-1cfc19` — Background non-persistent agent resume (handle_agent_resume) — follow-up split from bd-14114e
- (parent: `bd-14114e` — async lifecycle endpoints; reference commits 4b7018f5d6, 8ae1e5232d on main)

## Before state

- `handle_agent_resume` ran `try_resume_with_auto_retry` (Pi launch + MCP +
  session load) synchronously; a slow resume could exceed
  DEFAULT_AGENT_LIFECYCLE_REQUEST_TIMEOUT_SECS (120s) and transport-time-out.
- No request-body / background option; no handler-level resume HTTP test.

## After state

- Resume backgrounds by default and returns 202 accepted immediately; drives to
  Running or a terminal Failed in the background.
- The not-found -> remote-forward path stays synchronous (locality-gated).
- caco-daemon lib compiles; `should_background_resume` unit test passes; clippy
  introduces no new warnings (pre-existing warnings unchanged).

## Diff summary

- Code commit: see reintegration receipt for the landed squash SHA.
- Files touched: `crates/caco-daemon/src/lib.rs` (+~210/-~94).
- Added: `AgentResumeRequestBody { background }` (optional, default true; the CLI
  already POSTs `{}` so it is backward-compatible); pure `should_background_resume`
  decision fn (+ unit test); `finalize_resumed_agent` helper extracting the
  Ok-arm bookkeeping (sentinel sync, UI session update, AgentResumed feed event,
  audit log) so the spawned task runs identical side effects; the
  accept-and-background branch returning 202 with status/attach guidance; bg
  Err arm records terminal Failed + failed UI event (bd-435a8b stranding guard).
- Tests: +1 unit test (`should_background_resume_defaults_true_only_when_local_bd_1cfc19`).
- Behavioural delta: default `caco agent resume` now returns 202 (accepted) and
  resumes in the background; `{"background": false}` keeps the legacy synchronous
  200. The not-found→forward path is unchanged.

## Operator-takeaway

This completes the bd-14114e lifecycle-async family: refresh, both recreate
paths, and now resume all return promptly and do the slow Pi work in the
background, so first-party lifecycle POSTs stay responsive under the 120s
timeout. The only operator-visible change is that `caco agent resume` returns
"accepted" immediately instead of blocking until the agent is back; poll
`caco agent status` (or attach) to see it reach Running. Resume failures still
land a terminal Failed state so a backgrounded failure can't masquerade as a
stuck Starting agent.
