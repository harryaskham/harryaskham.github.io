# Session summary — bd-1e3227: caco msg reply auto-resolve + existence check; remove vapor 'msg thread' reference

## Goal

Three msg-surface issues filed by test-user-hel:

1. `caco msg reply --target` documented as "defaults to
   the sender of the original message" but
   implementation hard-errored "auto-resolve from
   message-id not yet implemented". Documentation lied.
2. `caco msg send --help` referenced `caco msg thread`,
   but no such subcommand exists.
3. `caco msg reply --message-id <bogus>` silently
   created an orphan reply with `parent_msg_id=msg-bogus`
   persisted to the chat log.

## Bead(s)

- `bd-1e3227` — test-user filed. Closed.

## Before state

```
$ caco msg reply --message-id msg-real --body x
error: --target is required for msg reply (auto-
resolve from message-id not yet implemented)

$ caco msg send --help | grep -i thread
--reply-to ... surfaced in inbox + caco msg thread
$ caco msg thread --help
error: unknown command path for help: caco msg thread

$ caco msg reply --message-id msg-bogus --body x --target foo
reply unknown to msg-bogus in project cacophony   # silent orphan
```

## After state

```
$ caco msg reply --message-id <real> --body x       (no --target)
reply msg-... to msg-... in project cacophony
(target: winmini:cacophony:winmini-cacophony-caco-dev-wmi-1)
                            ^^^ auto-resolved from sender of parent

$ caco msg send --help | grep -i thread
--reply-to ... surfaced in inbox via reply_to /
parent_msg_id fields
$ caco msg thread --help
(unchanged — never existed; the dangling reference
in --reply-to summary is gone.)

$ caco msg reply --message-id msg-bogus --body x --target foo
error: message-id 'msg-bogus' not found in project
'cacophony' (cannot reply to a message that does not
exist)
```

## Diff summary

- 1 file touched, +60 / −12:
  - `crates/caco-cli/src/lib.rs::dispatch_msg_reply`:
    - Pre-flight GET `/api/v1/messages/{id}/status`
      (handle_msg_status, lib.rs:15742) to validate
      existence + extract original sender.
    - On `ok=false` + `code=not_found`, surface a
      friendly "message-id … not found" error.
    - Cross-project safety: if the message belongs to
      a different project, error with the suggested
      `--project` correction.
    - When `--target` not supplied, auto-resolve from
      `original.data.sender`. Help text already
      promised this; previously errored.
    - Success message now includes "(target: …)" so
      operator can see which target was resolved.
  - `crates/caco-cli/src/lib.rs::SEND_ARGS --reply-to`:
    summary updated to remove the dangling "caco msg
    thread" reference. Replaced with "surfaced in
    inbox via reply_to / parent_msg_id fields" which
    is what's actually true today.

## Verification

- `cargo build --bin caco`: clean.
- TEST 1 (bogus id): friendly "message-id 'msg-bogus'
  not found" error — no orphan persisted.
- TEST 2 (real id, no --target): self-tested by
  speaking a broadcast, then replying without
  --target → auto-resolved to wmi-1 (the sender);
  reply was injected into wmi-1's runtime live.
- TEST 3 (--help text): "caco msg thread" reference
  removed from --reply-to summary.

## Operator-takeaway

Two-step "GET status, then POST send" is the right
pattern when an operation needs to validate a
foreign-key reference. Cost is one extra round-trip
(local daemon, ~1ms). The previous behavior
(silent orphan) cost much more in operator confusion
+ stale parent_msg_id rows in the chat log.

The "auto-resolve target from sender" pattern can be
reused for `msg send --reply-to` (currently requires
explicit --target even when --reply-to is present).
Filing follow-up if operator asks; for now reply is
the documented auto-resolve surface.

For vapor-help references (issue 2): `caco msg --json`
already lists real subcommands; a CI test that
greps --help text for `caco <verb> <noun>` shapes
and asserts each is reachable would catch this
class of drift. Filed mentally as a "future
operator-takeaway" — not blocking.
