# Session summary 0048 — bd-939541: choices --notify-mode (slice 1)

## Goal

Schema + CLI surface for choice-notification escalation, so AFK
operators don't miss critical choice-presentations.

## Bead(s)

- `bd-939541` slice 1 — schema + CLI flag only.

## Before state

- `caco choices present` could only signal via speak (broadcast).
- Tonight cluster-ctrl's threshold-cross choice sat ~2h waiting
  on AFK operator.

## After state

- `caco choices present --notify-mode <mode>` flag.
- Allowed values (validated client-side): `speak`,
  `direct-message`, `broadcast`, `escalate`, `timeout-fallback`.
- `ActiveChoice.notify_mode: Option<String>` field (serde
  default + skip_serializing_if).
- `PresentChoicesRequest` and `PresentChoiceRequest` both gain
  `notify_mode` field with serde default.
- Both `/api/v1/choices/present` handlers thread
  `req.notify_mode` into the new ActiveChoice.
- All ActiveChoice literals (2 handlers + 10 test fixtures)
  updated; tests pass.

## Diff summary

- Commit: `9c02b37b`.
- Files (4): caco-cli lib.rs, caco-daemon choices.rs + lib.rs +
  operator_inbox.rs.
- 17 caco-daemon::choices tests pass; clippy clean.

## Operator-takeaway

Agents can now declare how a choice should be surfaced. Slice 2
(supervisor reads `notify_mode` at present time and routes:
direct-message via `caco msg send`, broadcast via the broadcast
verb, escalate via external hook, timeout-fallback paired with
bd-ab376b's `afk_fallback_index`) is the next-step bead when
demand surfaces.
