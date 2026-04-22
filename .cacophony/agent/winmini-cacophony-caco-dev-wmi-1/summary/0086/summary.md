# Session summary — bd-be23e4: agent get/set extract daemon error.message; introspect rephrases 'unknown section'

## Goal

Test-user filed 3-issue bead.  Issues 1 + 3 are
reproducible CLI honesty bugs; Issue 2 was the
side-effect of a transient daemon-restarting state
that the operator-side machinery handles via the
`daemon_restarting` envelope (--json showed the right
shape) — not a real bug in agent stop's error path,
so deferred from this fix.

## Bead(s)

- `bd-be23e4` — test-user filed.  Closing for issues
  1 + 3.  Issue 2 was a transient repro artefact,
  noted in the close comment.

## Before state

```
$ caco agent get --id X --field state
error: agent get failed (400): {"ok":false,"error":
{"code":"field_get_failed","message":"daemon error:
unknown agent field: state; supported fields:
short_name, emotion, annotation"},"meta":{...}}

$ caco agent set --id X --field nonexistent --value foo
error: agent set failed (400): {"ok":false,"error":
{...same shape...}}

$ caco agent introspect --id X --show notarealsection
error: --show notarealsection not yet implemented
(supported: profile, hooks, env, tools)
                  # 'not yet implemented' implied future support
```

## After state

```
$ caco agent get --id X --field state
error: daemon error: unknown agent field: state;
supported fields: short_name, emotion, annotation

$ caco agent set --id X --field nonexistent --value foo
error: daemon error: unknown agent field: nonexistent;
supported fields: short_name, emotion, annotation

$ caco agent introspect --id X --show notarealsection
error: --show: unknown section 'notarealsection'
(supported: profile, hooks, env, tools)
```

## Diff summary

- 1 file touched, +35 / −10:
  - `crates/caco-cli/src/lib.rs`:
    - new `extract_daemon_error_message(body: &str)
       -> Option<String>` helper near
      `validate_non_negative_int`.  Pulls
      `error.message` from the standard daemon JSON
      envelope; returns None if the body isn't JSON
      or the field is missing/empty so callers can
      fall back gracefully.
    - `dispatch_agent_set` + `dispatch_agent_get`:
      use the helper before falling back to the raw
      `(status): {body}` shape.
    - `dispatch_agent_introspect`: rephrased "not yet
      implemented" → "unknown section '<x>'".  The
      old wording falsely implied future support.
    - test
      `agent_introspect_rejects_unsupported_show_section`
      updated: previously checked the "tools not yet
      implemented" string but tools is now supported
      (bd-340a4f), so the test was relying on a stale
      wording.  Now uses 'notarealsection' and asserts
      both the unknown-section message + the supported
      list.

## Verification

- `cargo build --bin caco`: clean.
- `cargo test -p caco-cli --lib agent_introspect`: 3
  passed (incl. the rewritten test).
- All 5 cases (3 fix + 2 regression) verified live.

## Operator-takeaway

The "extract daemon error.message" pattern is the
non-bd counterpart to bd_cli_error / bd_daemon_result
which already handle this for bd-shaped surfaces.
Worth a sweep grep for `failed ({status}): {body}`
patterns across non-bd dispatchers — likely 2-5
more callsites that should adopt the helper.

The bd-5d2d83 test was a hidden time-bomb: it
asserted on a string ("tools not yet implemented")
that became false when bd-340a4f added tools
support.  Tests that pin exact error wording for
unsupported features need to use a value that's
guaranteed-unsupported (here: a synthetic
'notarealsection' string).  Lesson: prefer asserting
on stable error shape ("unknown section X") over
prose subject to landing future support.

Issue 2 of bd-be23e4 (`caco agent stop --id
nonexistent` → 'daemon request failed') was a real
transport-error envelope but appeared during an
unrelated daemon restart cycle on winmini.  The
--json path correctly surfaces `daemon_restarting`
with bind ETA so the actual error path is sound.
The text mode could be improved to detect 401/404
shapes more specifically, but that's a separate fix
that needs daemon-up-not-restarting to verify; not
in scope here.
