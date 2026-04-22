# Session summary — bd-2d3d19: agent list / msg inbox project-name validation parity

## Goal

`caco bd list` validates `--project` against the
configured set; `caco agent list` and `caco msg inbox`
did not. Operator typing the wrong project saw "no
agents" / "no unread messages" — silent and misleading.

## Bead(s)

- `bd-2d3d19` — own bead. Closed.

## Before state

```
$ caco agent list --project bogusproj
caco agent list — no agents             # silent

$ caco msg inbox --project bogusproj
no unread messages                      # silent

$ caco bd list --project bogusproj
error: project 'bogusproj' is not configured...   # validates
```

## After state

```
$ caco agent list --project bogusproj
error: unknown project: bogusproj. Defined: a.skh.am,
cacophony, collective, gfx-replacer, midi2hid, mono,
picasso-health, tendril

$ caco msg inbox --project bogusproj
error: unknown project: bogusproj. Defined: ...

$ caco msg inbox --project cacophony --limit 1   (real result, unchanged)
$ caco agent list                                (no --project, unchanged)
```

## Diff summary

- 1 file touched, +12 / −0:
  - `crates/caco-cli/src/lib.rs`:
    - `agent list` dispatch arm: when `--project` was
      supplied explicitly (not env, not implicit),
      run `validate_project_name`.
    - `msg inbox` dispatch arm: same gating.

## Verification

- `cargo build --bin caco`: clean.
- agent list / msg inbox with `--project bogusproj`
  → `unknown project` error with full enumeration.
- agent list / msg inbox with valid `--project`
  → unchanged real results.
- agent list with no `--project` (uses CACO_PROJECT env)
  → unchanged (validation skipped per the
  `flags.contains_key("--project")` gate).

## Operator-takeaway

The contains_key-gate matters: `resolve_project_from_flags_or_env`
looks at flags AND env, so just checking the resolved
result would also validate the env-derived project name.
That's potentially a foot-gun for operators with stale
CACO_PROJECT but is generally fine. The conservative
read here is: explicit `--project` arg = explicit error
on typo; env-derived = trust the env.

If a future bead asks for "validate env-derived project
too", the gate is one line removal. For now, parity with
bd surface achieved without breaking workflow for
operators with valid CACO_PROJECT pointing at a
since-renamed/removed project.
