# Session summary — bd-05da5b: caco bd list --status enum validation

## Goal

`caco bd list --status <bogus>` silently returned the
full unfiltered list (mostly closed beads, since
closed > open in the database). 9th in the silent
unknown-value family.

## Bead(s)

- `bd-05da5b` — own bead. Closed.
  (The fleet-snapshot --format part of the bead's
  description turned out to be a false alarm — fleet
  snapshot has no --format flag at all; the unknown
  flag was silently dropped at parse stage, which is
  a different bd-b76723-family issue.)

## Before state

```
$ caco bd list --status nope --limit 3
bd-01aba0  P1  bug  closed  ...   # silent fall-through
bd-02b7c6  P1  feature  closed  ...
bd-023110  P1  bug  closed  ...
```

## After state

```
$ caco bd list --status nope
error: unknown --status value 'nope'. Allowed: open,
in_progress, closed, deleted, draft, permanent, blocked
```

Allowed set sourced directly from
`caco_beads::model::BeadStatus::parse` plus the computed
`blocked` label (bd-379b93). All 7 documented values
that the daemon will accept are listed in the error.

## Diff summary

- 1 file touched, +14 / −2:
  - `crates/caco-cli/src/lib.rs::dispatch_bd_list`:
    `--status` validated against the BeadStatus enum +
    `blocked` computed label; uses `bd_cli_error` for
    JSON-aware error envelope (the bd surface returns
    structured error JSON).

## Verification

- `cargo build --bin caco`: clean.
- `caco bd list --status nope` → enum error.
- `caco bd list --status open --limit 1` → real result.

## Operator-takeaway

9th in the silent-unknown-value family
(bd-126b99/bd-a403a1/bd-30fbfb/bd-2886bb/bd-dfc91a/bd-bc52ef
+ bd-c061d4/bd-3656ce/bd-40907c/bd-33d37c/bd-b617cc/bd-05da5b).

`bd_cli_error` is the right error helper for surfaces
that already return structured JSON envelopes — keeps
JSON consumers happy while text branch still gets the
human-readable message. Worth promoting to convention
when adding enum validation to other bd-* surfaces.

The validate_enum_flag helper (operator-takeaway from
summaries 0061+0066) would now have a 9th caller — but
the call shape varies between dispatch surfaces (some
return `CliError`, some return `Result<Outcome>` and
need `bd_cli_error`). Better-shaped refactor: a helper
that returns `Option<String>` (the error message) and
let each call-site choose its error envelope. Filing
follow-up if a 10th appears.
