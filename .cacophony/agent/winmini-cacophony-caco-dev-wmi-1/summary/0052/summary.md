# Session summary — bd-2a744a: caco mode {show,set,clear} validate --project

## Goal

Stop `caco mode show/set/clear --project <bogus>` from
silently creating or operating on per-project state for
a project that doesn't exist. A typo like `cacaphony`
should hard-error like every other project-validating
command, not silently succeed.

## Bead(s)

- `bd-2a744a` — P3 bug, test-user-hel filed.

## Before state

- `caco mode set burndown --project nonexistent-project`
  succeeded silently, creating an override.
- `caco mode show --project typo-project` returned
  Source=override after the fake create.
- `caco mode clear --project typo-project` happily
  cleaned up the made-up state.
- Compare to `caco config sparse show --project X` which
  correctly errored "unknown project: X".

## After state

- All three dispatchers (`dispatch_mode_show`,
  `dispatch_mode_set`, `dispatch_mode_clear`) now call
  the new `validate_project_name` helper before any
  daemon round-trip.
- Error format: `unknown project: <name>. Defined: a, b, c`
  (or `(no projects configured)` if zero).
- Helper lives next to `load_config_for` and is reusable
  by any future dispatcher with the same family of
  silent-create footguns.

## Diff summary

- 1 file touched, +50 / −0:
  - `crates/caco-cli/src/lib.rs`: 3 dispatcher
    pre-validates + new `validate_project_name` helper.

## Verification

- `cargo build -p caco-cli`: clean.

## Operator-takeaway

Family with bd-126b99/bd-a403a1/bd-30fbfb (CLI honesty
pass) — silent-success-on-nonsense-input is the same
class of bug. New helper is reusable; future dispatchers
with `--project` flags should use it.
