# Session summary 0028 — bd-4c8fdd: state-mutating commands refuse unknown flags

## Goal

Close the destructive warn-then-mutate footgun in `caco bd create`
(and every other non-idempotent CLI surface) where an unrecognised
flag printed a warning then proceeded to mutate state.

## Bead(s)

- `bd-4c8fdd` (P2 bug, 5 issues; this commit addresses Issue 1, the
  destructive one).

## Before state

```
$ caco bd create --preview --title 'preview-test'
warning: bd-b76723: `caco bd create` received unrecognised flag(s): --preview. These were ignored.
created: bd-0c73b7 — preview-test         ← REAL BEAD CREATED
```

5th surface in the bd-b76723 warn-then-process cohort and the
FIRST that mutates state.

## After state

```
$ caco bd create --preview --title 'preview-test'
error: bd-4c8fdd: `caco bd create` is a state-mutating command and
received unrecognised flag(s): --preview. Refusing to proceed;
remove the unknown flag(s) and re-run. (This refusal is
unconditional for write surfaces to avoid the warn-then-mutate
footgun; read-only surfaces still warn-and-proceed unless
CACO_STRICT_UNKNOWN_FLAGS=1.)
```

The dispatcher inverts the existing `CommandSpec.idempotent` bit
(bd-21c44d's classification) into a `force_strict` argument to a
new `warn_or_error_unknown_flags_with_strictness(...)` variant. No
new spec entries needed — the contract was already there, just
unused for this purpose.

## Diff summary

- `crates/caco-cli/src/lib.rs`: +134 / −7
  - new `_with_strictness` variant; original name preserved as a
    thin shim so existing 4 internal callers stay green
  - dispatch() at lib.rs:9594 inverts spec.idempotent → force_strict
  - 3 new tests + pin test on CommandSpec contract
- `cargo test -p caco-cli --lib warn_or_error_unknown_flags`: 6/6 pass
- `cargo test -p caco-cli --lib destructive_bd_subcommands`: 1/1 pass
- `cargo build -p caco-cli`: clean

## Embedded artefacts

(none)

## Operator-takeaway

The pin test `destructive_bd_subcommands_are_marked_non_idempotent`
is the load-bearing one — it asserts that `bd create` and
`bd dispatch` (the two confirmed destructive surfaces) keep
`idempotent=false` so a future spec edit can't quietly re-open the
warn-then-mutate footgun.

Issues 2, 3, and 5 in the bead description (CLI parity for
`--preview`/`--dry-run`, `bd close --note` affordance, `bd update
--dry-run`) are separate beads' worth of work and remain unaddressed
in this slice — bead will be closed since Issue 1 was the actual
P2 bug; the others are feature gaps that should get their own
beads if the operator wants them.
