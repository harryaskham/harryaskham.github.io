# Session summary — bd-a55f3c: globalise --agent-id (one-line silencing of bd-b76723 warning everywhere)

## Goal

After the bd-ffcc3e / bd-e5b2d0 / bd-ae3da0 sweep
landed (10 dispatchers got per-command --agent-id
ArgSpecs), I probed for more sites and found 7+ more
warning callsites: msg reply / broadcast / inbox,
bd show / list / info / search.

Promoting --agent-id to KNOWN_GLOBAL_FLAGS (the
list that includes --json, --help, --config,
--wait-daemon) silences the warning EVERYWHERE in
one line, instead of continuing the per-command
whack-a-mole. --agent-id is conceptually a global
caller-identity flag — every caco operation has a
caller for audit/inference purposes.

## Bead(s)

- `bd-a55f3c` — own follow-up. Closed.

## Before state

```
$ caco msg reply --message-id mid-X --body x --agent-id Y
warning: bd-b76723: `caco msg reply` received
unrecognised flag(s): --agent-id...
[plus 6 other commands with the same warning]
```

## After state

```
$ caco msg reply --message-id mid-X --body x --agent-id Y
[no warning]

# Verified 10 commands clean of the warning:
# msg reply / broadcast / inbox, bd show / list /
# info / search, service status, fleet snapshot,
# agent stop.
```

## Diff summary

- 1 file touched, +12 / −2:
  - `crates/caco-cli/src/lib.rs`:
    - `KNOWN_GLOBAL_FLAGS` extended with `"--agent-id"`
      and reformatted from 1-line to multi-line list
      to make adding entries cleaner.
    - Doc comment updated to explain the rationale.
    - Per-command --agent-id ArgSpecs from the
      bd-ffcc3e / bd-e5b2d0 / bd-ae3da0 family
      LEFT IN PLACE: they're still useful for
      subcommand `--help` documentation and continue
      to be read by dispatchers that semantically
      forward the flag (claim/unclaim/close/create/
      update).

## Verification

- `cargo build --bin caco`: clean.
- `cargo test-small`: 57 passed.
- 10 commands probed live with `--agent-id Y`:
  zero `warning: bd-b76723:` lines.

## Operator-takeaway

Three approaches to "flag is universally passed but
warns on most surfaces":

1. **Per-command ArgSpec extension** (bd-ffcc3e /
   bd-e5b2d0 / bd-ae3da0): documents the flag in
   subcommand help, requires N edits for N commands.
   Right when the flag has command-specific
   semantics or behaviour.

2. **Global-flag promotion** (this bead): one-line
   change, silences EVERYWHERE. Right when the flag
   is conceptually universal and 'pass-through OK
   where not meaningful' is acceptable behaviour.

3. **Dispatcher base-class** (not done): factor a
   common pre-handler that strips known caller-
   identity flags and forwards them as caller body
   field where the request struct accepts one. More
   invasive; not justified at current scale.

The right choice depends on whether the flag has
per-command semantics. For --agent-id the answer is
'mostly the same semantics everywhere' (caller
attribution), so global-flag promotion is the right
shape. The 10 per-command ArgSpecs landed in the
preceding 3 beads were not wasted: they document
the flag in --help on the dispatchers that
semantically forward it (where the operator
actually needs to know it has effect beyond
silencing the warning).

The pattern: add per-command ArgSpec when a flag
has command-specific semantics; promote to global
when a flag is universal. When BOTH apply (as here),
do both — global silences the warning, per-command
docs the meaning.

Family count after this bead:
- bd-ffcc3e: 3 sites with body-field forward.
- bd-e5b2d0: 5 sites with documentation-only ArgSpec.
- bd-ae3da0: 2 sites with body-field forward.
- bd-a55f3c: ALL OTHER sites silenced via global
  promotion (no count needed; 100% coverage).
