# Session summary — bd-bba6b0: caco test/build list accept --status as alias for --state

## Goal

Eliminate the cross-cutting flag-name inconsistency where the
`list-by-X` family splits between `--status` (caco bd, choices,
release, outbox) and `--state` (caco ps, test, build). Operators
who type `caco test list --status queued` (natural transfer
from sister surfaces) hit the bd-b76723 unrecognised-flag
warning and silent unfiltered output.

## Bead(s)

- `bd-bba6b0` — `caco test/build use --state filter while
  bd/choices/release use --status — cross-cutting flag-name
  inconsistency in list-by-X family; --status on test/build
  silently fires bd-b76723 instead of suggesting --state`.

## Before state

- `caco test list --status queued` →
  `warning: bd-b76723: caco test list received unrecognised
  flag(s): --status. ...` then unfiltered output.
- `caco build list --status queued` → same.
- Operator can't tell whether the queue is empty or their flag
  was wrong.

## After state

`TEST_LIST_ARGS` and `BUILD_LIST_ARGS` declare a `--status`
ArgSpec with the summary `Alias for --state (accepted for
parity with caco release/bd/choices list).` so:

1. `caco {test,build} list --help` documents the alias.
2. `validate_flags` / bd-b76723 unrecognised-flag detection
   accepts it without a warning.

`dispatch_test_list` and `dispatch_build_list` lift the alias:

- `flags.get("--state").or_else(|| flags.get("--status"))` →
  the existing enum-validator and query-string code is
  unchanged downstream.
- Mutual-exclusion guard:
  `error: test list: --status and --state are aliases; pass
  only one` (consistent with bd-b76723 mutex style).

The alias is the bead's option (a) — the dispatcher-level
flag-aliases approach. Option (b) (pick canonical and migrate)
was rejected because changing established `--status` on
release breaks scripts; option (c) (just better hint text) was
weaker. Aliases let us fix the operator experience without
breaking either side.

## Diff summary

- `crates/caco-cli/src/lib.rs`:
  - `TEST_LIST_ARGS`: added `--status` ArgSpec with
    "Alias for --state (...)" summary.
  - `BUILD_LIST_ARGS`: same.
  - `dispatch_test_list`: added mutex guard + `or_else` lift
    of `--status` into the `--state` code path.
  - `dispatch_build_list`: same.
  - 2 new tests:
    - `test_list_and_build_list_advertise_status_alias_for_state`
      — runs `--help --json` for both subcommands and asserts
      the `--status` arg appears with summary containing
      "Alias for --state".
    - `dispatch_test_and_build_list_lift_status_alias_to_state`
      — source-greps both dispatch bodies for the `or_else`
      lift and the mutex error wording so a future refactor
      can't silently delete the alias and re-introduce the bug.
- `cargo test -p caco-cli --lib -- ...`: both new tests pass.
- `cargo test-small`: 162 pass.

## Operator-takeaway

This is the first dispatcher-level flag-alias landing in the
bd-2a4552 / bd-b7392e / bd-fca3e1 family. The pattern
generalises: a documented `Alias for --X` ArgSpec entry +
`or_else` lift in the dispatcher + mutex guard is enough to
cover any sister-surface flag-name divergence without
breaking either spelling.

The bead lists three other issues (Issues 2–5) deliberately
not addressed here:

1. **--job-id / --release-id / --entry-id / --id alias gap
   on test show, build show** (Issue 2) — same pattern
   could land in TEST_SHOW_ARGS / BUILD_SHOW_ARGS but is
   distinct work belonging with the bd-3a6078 family.
2. **Issue 3** is positive feedback (test/build are
   high-quality), no action needed.
3. **Issue 4** (test/build queues empty operationally) is an
   ops/UX question, not a bug.
4. **Issue 5** (test list --status silent degrade) is the
   exact bug this fix addresses — no longer applies.

Issue 2 would be a natural follow-up bead; everything else
is fully covered or out of scope.
