# Session summary — bd-e5b2d0: --agent-id documentation-only ArgSpec on 5 commands (silences bd-b76723 warnings)

## Goal

Sibling of bd-ffcc3e. 5 callsites where --agent-id
triggered the bd-b76723 unrecognised-flag warning:

  agent stop, agent nudge, agent reintegrate,
  msg speak, msg send

For all 5 the daemon-side endpoint takes caller from
the X-Caco-Caller header (not a body field), so semantic
forwarding is a deeper change than the body-field
forward used in bd-ffcc3e. This bead does
documentation-only ArgSpec extension to silence the
warning + document the flag in `--help`; semantic
forwarding deferred to a follow-up.

## Bead(s)

- `bd-e5b2d0` — own follow-up. Closed.

## Before state

```
$ caco agent stop --id X --agent-id Y
warning: bd-b76723: `caco agent stop` received
unrecognised flag(s): --agent-id. These were ignored
by the dispatcher.
[stop succeeds via env-derived caller]
```

## After state

```
$ caco agent stop --id X --agent-id Y
[stop succeeds, no warning]

$ caco agent stop --help
  --agent-id   Caller agent ID (defaults to env
               CACO_AGENT_ID). Currently informational.
```

## Diff summary

- 1 file touched, +50 / −0:
  - `crates/caco-cli/src/lib.rs`:
    - `AGENT_REINTEGRATE_ARGS`, `AGENT_STOP_ARGS`,
      `AGENT_NUDGE_ARGS`, `MSG_SEND_ARGS`,
      `MSG_SPEAK_ARGS`: appended new `--agent-id`
      ArgSpec entries.  Help text labelled "Currently
      informational" so operators know the flag is a
      no-op pending the semantic-forwarding follow-up.

## Verification

- `cargo build --bin caco`: clean.
- `cargo test-small`: 57 passed.
- 5 cases verified live: each command no longer
  emits the bd-b76723 warning when --agent-id is
  supplied.  `--help` lists the flag.

## Operator-takeaway

This is a 'silence-the-warning' fix, not a 'change-
the-behaviour' fix. The two are worth distinguishing:
- bd-ffcc3e (claim/unclaim/close): silence-warning +
  semantically forward via body 'caller' field.
- bd-e5b2d0 (this): silence-warning ONLY. Semantic
  forwarding requires daemon-side changes or
  per-dispatcher header overrides; deferred.

Both fixes share the property that operators were
already passing the flag and getting the warning;
both eliminate the warning. The semantic divergence
is documented in the help string ("Currently
informational") so operators can rely on env-derived
caller for now and switch to --agent-id when the
follow-up lands.

Follow-up bead candidates (file when scope is clear):
- "[CLI honesty] --agent-id semantically authoritative
  for caller (override X-Caco-Caller header) on agent
  stop/nudge/reintegrate + msg speak/send" — depends
  on either CLI header override or daemon-side struct
  extension, ideally the former for back-compat.

Side note: tried to caco bd update --description on
the in-progress bead and got a misleading "cannot be
in_progress with no assignee" error even though show
displayed the assignee correctly.  Possible daemon-
side glitch during my probing; not blocking.  Worth
flagging if it reproduces.
