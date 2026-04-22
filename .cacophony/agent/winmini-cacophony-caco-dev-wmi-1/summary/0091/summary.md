# Session summary — bd-ae3da0: bd create + bd update accept --agent-id and forward as caller

## Goal

Sibling of bd-ffcc3e (claim/unclaim/close) and
bd-e5b2d0 (agent stop/nudge/reintegrate + msg
speak/send).  Two more bd-* dispatchers warned on
--agent-id:

  bd create --project P --title T --agent-id Y
  bd update --bead-id bd-X --agent-id Y

Both daemon-side request structs (`CreateBeadRequest`,
`UpdateBeadRequest` in caco-daemon/src/beads.rs)
already accept a `caller: Option<String>` body
field — the CLI just wasn't forwarding it.  Same
shape as bd-ffcc3e.  This bead matches that shape:
ArgSpec extension + body forward + no-op
preservation.

## Bead(s)

- `bd-ae3da0` — own follow-up. Closed.

## Before state

```
$ caco bd create --project P --title T --agent-id Y
warning: bd-b76723: `caco bd create` received
unrecognised flag(s): --agent-id...
created: bd-XXXX — T

$ caco bd update --bead-id bd-X --agent-id Y --add-label foo
warning: bd-b76723: `caco bd update` received
unrecognised flag(s): --agent-id...
[update succeeds with env-derived caller]
```

## After state

```
$ caco bd create --project P --title T --agent-id Y
created: bd-XXXX — T

$ caco bd update --bead-id bd-X --agent-id Y --add-label foo
[succeeds with explicit caller Y]

$ caco bd update --bead-id bd-X --agent-id Y    # no field
error: bd update requires at least one field flag
(--title, ...)
```

## Diff summary

- 1 file touched, +25 / −0:
  - `crates/caco-cli/src/lib.rs`:
    - `BD_CREATE_ARGS`: appended `--agent-id` ArgSpec.
    - `BD_UPDATE_ARGS`: same.
    - `dispatch_bd_create`: forward `--agent-id` as
      `body["caller"]` after the existing field
      builders.
    - `dispatch_bd_update`: same forward, but placed
      AFTER the bd-f758f7 no-op check so `--agent-id`
      alone still triggers the missing-fields error
      (--agent-id is metadata, not a mutation field).

## Verification

- `cargo build --bin caco`: clean.
- `cargo test-small`: 57 passed.
- 3 cases verified live:
  - `bd create --agent-id Y`: no warning, created
    successfully.
  - `bd update --add-label foo --agent-id Y`: no
    warning, update succeeded.
  - `bd update --agent-id Y` (no field): still errors
    with no-op message — the metadata flag did NOT
    silently turn the call into a no-op-success.

## Operator-takeaway

The body-field forwarding pattern (vs. header
override) is the right choice when the daemon-side
struct already exposes a `caller` field. It's
strictly additive at the CLI and changes nothing for
operators not passing --agent-id.

The "place metadata-flag forwarding AFTER any
no-op / required-field validation" rule matters:
otherwise --agent-id alone could:
- Build a body containing only {caller: Y}
- Succeed at the daemon as a vacuous PATCH (no
  fields changed but a caller is recorded)
- Hide the operator's mistake of forgetting an
  actual mutation flag.

The bd-f758f7 client-side check guards against this
already; positioning the --agent-id forward AFTER
that check preserves the guard.

Family count after this bead:
- bd-ffcc3e: 3 sites with body-field forward
  (claim/unclaim/close).
- bd-e5b2d0: 5 sites with documentation-only
  silencing (agent stop/nudge/reintegrate, msg
  speak/send).
- bd-ae3da0: 2 sites with body-field forward
  (create/update).

Total: 10 dispatchers now accept --agent-id without
warning. Deferred: header-override semantic
forwarding for the bd-e5b2d0 cluster.
