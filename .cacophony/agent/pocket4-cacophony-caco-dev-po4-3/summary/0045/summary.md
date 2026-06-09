# Session summary — env-gate Pi wake extensions (pause/managed), slice 1

## Goal

Fix the operator-reported bug that PAUSED managed Pi agents still fire `/loop`,
still poll inbox, and still self-nudge, and start exposing the repo Pi extensions
through the repo-level Pi package env-gated to managed agents (bd-31f01c). This
slice lands the wake-path correctness fix; the full package.json manifest export
(after gating the enforce-policy guards) is slice 2.

## Bead

- `bd-31f01c` (P1, operator-reported) — Expose all repo Pi extensions in the
  repo-level Pi package, env-gated to activate only inside managed agents.
  Remains in_progress for slice 2.

## Before state

- Wake-path Pi extensions armed timers and injected `sendUserMessage` regardless
  of context: `caco-self-nudge` armed a 5-min timer + injected even when
  AGENT_ID was unknown; `caco-inbox` armed a 20s poll timer at session_start;
  none of loop/inbox/self-nudge honored the daemon pause stamp, so a PAUSED
  managed agent kept waking. `npm test` only ran one extension test file.

## After state

- New shared helper `caco-agent-env.mjs` (per-dir copy in self-nudge/inbox/loop,
  matching the existing per-dir caco-process-utils.mjs convention): `managedAgentId`,
  `isManagedAgent`, `pauseStampPath`, `isPausedSync` (sync, used on fire-and-forget
  timer paths to preserve delivery timing), and async `isPaused`.
- `caco-self-nudge`: `activate` returns early (fully dormant) outside a managed
  agent; `runPeriodicSelfNudge` skips the nudge while paused (records a
  `periodic_self_nudge_skipped reason=agent_paused`), still reschedules.
- `caco-inbox`: `session_start` only auto-arms the poll timer inside a managed
  agent (the `/caco-inbox` command stays available for explicit operator use);
  `runInboxTick` skips the whole tick while paused (records `paused_skip`) without
  fetching or advancing deliveredIds/seed state, so no inbox messages are lost
  across the pause.
- `caco-loop`: `runLoopTick` suppresses the repeating prompt injection while
  paused (sync pause check keeps delivery on the fire-and-forget timer's tick),
  still reschedules; operator `/loop` is unaffected outside a managed agent.
- `package.json` `scripts.test` now runs all `.cacophony/pi/**/extensions/*.test.mjs`
  and `plugins/caco-agent/pi-extensions/*.test.mjs` so the per-extension tests
  (incl. the new ones) actually run.
- `pi.extensions` manifest intentionally unchanged this slice (the full export
  waits until the enforce-policy guards — checkout-guard/speak-enforce/lifecycle —
  are gated in slice 2).

## Diff summary

- Code commits: pending final squash SHA from the reintegration receipt.
- Files: new `caco-agent-env.mjs` + `caco-agent-env.test.mjs` in
  `.cacophony/pi/{self-nudge,inbox,loop}/extensions/`; gating edits in
  `caco-self-nudge.mjs`, `caco-inbox.mjs`, `caco-loop.mjs`; new behavior tests in
  their `*.test.mjs`; `package.json` test-runner glob.
- Tests: +13 (9 helper unit tests, 4 behavior: inbox unmanaged-no-timer + paused
  skip, loop paused suppression, self-nudge unmanaged dormancy). `npm test`: 66
  pass / 0 fail.

## Operator-takeaway

A PAUSED managed Pi agent now receives no loop/inbox/self-nudge wakeups until
resume, with no inbox messages lost across the pause; the wake extensions are
also fully dormant in a plain operator Pi session. Slice 2 (gate the remaining
enforce-policy guards + export the full extension set in package.json so one
`pi install` carries the whole toolset) is still open on bd-31f01c.
