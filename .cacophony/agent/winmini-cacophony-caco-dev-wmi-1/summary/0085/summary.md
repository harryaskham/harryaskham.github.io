# Session summary — bd-cccf2f: bd update/create --type validates client-side via existing helper

## Goal

Probe sweep finds two callsites where `--type WRONG`
leaks the daemon's HTTP 422 parse-failure shape.  The
sibling `--priority` flag already validates client-side
via a dedicated helper; --type just didn't.

This is the 15th member of the silent-unknown-value
family.  Easy reuse of validate_enum_flag (bd-2c6856).

## Bead(s)

- `bd-cccf2f` — own follow-up. Closed.

## Before state

```
$ caco bd update --bead-id bd-X --type WRONG
error: invalid response (HTTP 422 Unprocessable Entity):
  expected value at line 1 column 1

$ caco bd create --project P --title T --type WRONG --priority P3
error: invalid response (HTTP 422 Unprocessable Entity):
  expected value at line 1 column 1
```

## After state

```
$ caco bd update --bead-id bd-X --type WRONG
error: unknown --type value 'WRONG'. Allowed: task,
bug, feature, epic

$ caco bd create --project P --title T --type WRONG --priority P3
error: unknown --type value 'WRONG'. Allowed: task,
bug, feature, epic

$ caco bd update --bead-id bd-X --type task     (legit)
(downstream semantic check still applies)
```

## Diff summary

- 1 file touched, +14 / −2:
  - `crates/caco-cli/src/lib.rs::dispatch_bd_update`:
    new `validate_enum_flag("--type", v, ALLOWED_TYPE)`
    call in the existing `--type` block.  On error
    routes through `bd_cli_error("invalid_argument",
    ...)` so JSON callers get the structured envelope.
  - `dispatch_bd_create`: identical shape applied
    to the `--type` block.

## Verification

- `cargo build --bin caco`: clean.
- All 3 cases in After state verified live via the
  dev binary.

## Operator-takeaway

15 instances of the silent-unknown-value family now
closed.  The pattern of "lift validation client-side
via validate_enum_flag" has reached the point where
the cluster's biggest remaining work is just finding
overlooked surfaces — there are no novel shapes to
add.

The "wrap CliError in bd_cli_error" idiom (4 lines:
`if let Err(e) = ... { return bd_cli_error(...) }`)
is starting to feel slightly over-ceremonial. Could
extract a `bd_validate_enum_flag(json_requested, ...)`
shim that does both, but only worth the change after
3-4 more callers — premature for one new use.

Probe surface for next round: --severity / --kind on
log-error / msg-broadcast / notify-create — same
shape, smaller universe of valid values, worth a
quick pass.

bd-940284 (msg send to nonexistent target) deferred
for operator weigh-in (target universe needs design
input).
