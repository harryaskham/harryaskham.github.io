# Session summary — bd-154a15: msg send --reply-to validation + --target auto-resolve; lookup helper extracted

## Goal

Follow-up to bd-1e3227 (msg reply got these fixes).
Same shape on the send surface:

1. `caco msg send --reply-to msg-bogus --target X --body x`
   silently created a reply with `parent_msg_id=msg-bogus`
   persisted to the chat log (orphan parent).
2. `caco msg send --reply-to <real>` required --target
   even though the parent's sender is the obvious
   default — same gap that bd-1e3227 fixed for reply.

## Bead(s)

- `bd-154a15` — own bead. Closed.

## Before state

```
$ caco msg send --reply-to msg-bogus --target X --body x
sent message unknown to X in project cacophony   # silent orphan

$ caco msg send --reply-to <real> --body x
error: --target is required for msg send         # no auto-resolve
```

## After state

```
$ caco msg send --reply-to msg-bogus --target X --body x
error: message-id 'msg-bogus' not found in project
'cacophony' (cannot thread under a message that does
not exist)

$ caco msg send --reply-to <real> --body x       (no --target)
sent message msg-... to winmini:cacophony:wmi-1 in
project cacophony                                 (auto-resolved)

$ caco msg send --body x                          (no --target, no --reply-to)
error: --target is required for msg send (or supply
--reply-to to auto-resolve from the parent message's
sender)

$ caco msg send --target X --body x               (unchanged path)
sent message msg-... to X in project cacophony
```

## Diff summary

- 1 file touched, +60 / −20:
  - `crates/caco-cli/src/lib.rs`:
    - Extracted `lookup_parent_message_for_threading`
      helper. Reusable by both reply and send paths.
      Returns `Result<Option<String>, CliError>` —
      Some(sender) if recorded, None if not, Err on
      not-found / cross-project / lookup failure.
    - `dispatch_msg_reply` rewritten to use the helper
      (was inline). Behavior unchanged.
    - `dispatch_msg_send` signature: `target: &str`
      → `target: Option<&str>`. Inside, a 4-way match
      on (target, reply_to) handles: both supplied
      (validate parent), target-only (unchanged),
      reply-to-only (auto-resolve), neither (friendly
      error suggesting --reply-to as alternative).
    - Dispatch arm (msg send) updated to make --target
      optional when --reply-to is present, with a
      friendlier error pointing at --reply-to as the
      alternative.

## Verification

- `cargo build --bin caco`: clean.
- TEST 1 (bogus reply-to + target): friendly
  not-found error, no orphan persisted.
- TEST 2 (bogus reply-to, no target): same — parent
  validation runs before any send.
- TEST 3 (real reply-to, no target): self-tested
  end-to-end. Speak broadcast → send reply with no
  --target → auto-resolved to wmi-1 (the sender);
  reply was injected into wmi-1's runtime live.
- TEST 4 (no reply-to, no target): friendly error
  pointing at --reply-to as the alternative.

## Operator-takeaway

The 'lookup-then-send' shape that bd-1e3227 introduced
turned out to be exactly right for one helper. Two
consumers (reply + send), shared error envelope,
one tested code path. If a third consumer appears
(e.g. `caco msg broadcast --reply-to`), the helper
already covers it.

For dispatch arms with multiple-flag combinatorics
(reply-to vs target), the (Option, Option) match
pattern reads cleanly and exhaustively. Cleaner than
nested ifs / fallback assignments. Worth promoting
when adding similar combinatorics elsewhere
(e.g. `--from-stdin` vs `--body` exclusivity in send).

The `target` parameter changing from `&str` to
`Option<&str>` is a minor API break, but
`dispatch_msg_send` is internal so no external impact.
The dispatch arm absorbs the change.
