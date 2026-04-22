# Session summary — bd-c95d44: bd list --sort validation

## Goal

14th in the silent-unknown-value family.
`caco bd list --sort bogus` silently used the default
Priority sort because daemon `parse_bead_sort`
(beads.rs:4895) catches unknowns with
`_ => BeadSortField::Priority`.

## Bead(s)

- `bd-c95d44` — own bead. Closed.

## Before state

```
$ caco bd list --sort bogus --limit 1
bd-01aba0  P1  ...   # silent default-sort
```

## After state

```
$ caco bd list --sort bogus
error: unknown --sort value 'bogus'. Allowed: priority,
created_at, created, updated_at, updated,
occurrence_count, occurrences, estimated_effort, effort,
worker_age, worker-age, claim_age

$ caco bd list --sort created_at --limit 1   (real result, unchanged)
$ caco bd list --sort worker_age --limit 1   (client-side sort, unchanged)
```

## Diff summary

- 1 file touched, +14 / −1:
  - `crates/caco-cli/src/lib.rs::dispatch_bd_list`:
    `--sort` validated via `validate_enum_flag` against
    daemon enum (priority/created_at|created/
    updated_at|updated/occurrence_count|occurrences/
    estimated_effort|effort) plus the client-side
    bd-5e75ee sorts (worker_age|worker-age, claim_age).

## Verification

- `cargo build --bin caco`: clean.
- `--sort bogus` → enum error with full allowed list.
- `--sort created_at` → real result (unchanged).
- `--sort worker_age` → client-side path still works
  (the validate_enum_flag check passes; the existing
  `if !matches!(...)` continues to skip the daemon
  param push).

## Operator-takeaway

14th in family, 5th use of `validate_enum_flag` —
helper continues to feel right.

Pattern note: when the validator's allowed-list and
the runtime's switch-on-string aren't in the SAME
crate (here client-side validates against daemon-
defined enum), there's a drift risk. Mitigation
options:
- Export `BeadSortField::all_aliases()` from
  caco-beads and consume it client-side. Cleaner but
  adds a CLI -> beads dependency the CLI may not
  otherwise need.
- Keep allowed-list co-located near the validator and
  add a daemon-side test that asserts
  `parse_bead_sort` accepts every alias the client
  documents.

Both deferred — the family is small enough that the
drift risk is contained, and adding a test would
itself be a multi-file change.
