# Slice 13 — bd-e64a83: caco msg send no longer reports 'sent message unknown' on failure

## Goal

Stop silently reporting "sent message unknown" when the daemon returns an error envelope from `caco msg send`.

## Bead(s)

- **bd-e64a83** (bug, P3) — caco msg send can report sent message unknown.

## Root cause

`dispatch_msg_send` did:

```rust
let msg_id = resp["data"]["id"].as_str().unwrap_or("unknown").to_owned();
```

then printed `"sent message {msg_id} to {target}"` regardless of whether the daemon actually accepted the send. When the daemon returned `{"ok":false,"error":{...}}` (e.g., store_error 500), `data.id` was absent → fallback `"unknown"` → operator saw a fake-success message.

## Before state

```
$ caco msg send --target X --body "hi"
sent message unknown to X in project cacophony
```

(no actual send; daemon error swallowed)

## After state

```
$ caco msg send --target X --body "hi"
error: msg send failed (store_error): <real error message from daemon>
```

If `ok=true` but `data.id` is somehow still missing, surfaces a daemon contract violation error including the response shape.

## Diff summary

```
 crates/caco-cli/src/msg_cmd.rs | ~25 lines added
 1 file changed
```

`cargo check -p caco-cli` clean.

## Operator-takeaway

Failed `caco msg send` calls now show the real error instead of "sent message unknown". Sister to bd-879ee1 (speak surface) but in the direct-send formatter.
