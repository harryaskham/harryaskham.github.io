# Session summary 0074 — bd-61aba9 cohort-wide empty-id fix

## Goal

Push the empty-id rejection into resolve_agent_id so ~30 dispatcher
sites are covered at once.

## Bead(s)

- bd-61aba9 (self-filed) — resolve_agent_id cohort fix

## Before state

- bd-fffbc1 fixed 4 surfaces with per-site calls; ~30 others remained
  leaky.
- agent pause --id '' silently paused all agents (including self).

## After state

- resolve_agent_id Err on explicit empty flags.
- Agent pause site-local guard added to prevent 'pause all'.
- Stale .map_err wrappers removed so the helper's Err propagates.

## Diff summary

- Commit: 1dca76461b59
- File: crates/caco-cli/src/lib.rs
- Tests: +1

## Operator-takeaway

All caco agent <sub> commands now error gold-standard on empty
--agent-id or --id instead of leaking into the daemon URL or
silently operating on every agent.
