# Session summary 0073 — bd-fffbc1 agent lifecycle empty-id retrofit

## Goal

Plug the URL-leak hole on caco agent stop/restart/resume/discard with
empty --agent-id, using the bd-29c7e3 shared helper.

## Bead(s)

- bd-fffbc1 (self-filed) — agent lifecycle empty-id leaks

## Before state

- 4 surfaces leaked '' into daemon URL (HTTP route leak shape) or
  hit the daemon and timed out.

## After state

- All 4 reject up-front with the gold-standard
  '--id must not be empty for caco agent X (list available: caco agent list)'.

## Diff summary

- Commit: 377358f7a7eb
- File: crates/caco-cli/src/lib.rs (4 dispatcher branches)

## Operator-takeaway

Try: env -u CACO_AGENT_ID caco agent stop --agent-id ''
Now errors gold-standard instead of leaking the URL.
