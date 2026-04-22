# Session summary — caco bd list --title-contains (bd-f64f70)

## Goal

Generalize the title-based filtering from bd-8fe920
(operator-actions list) into a composable flag on
`caco bd list`.

## Bead(s)

- `bd-f64f70` — caco bd list --title-contains (P3 feature)

## Before state

- No title-based filter on `caco bd list`.
- Filtering by title required a dedicated subcommand or
  piping to grep.

## After state

- `caco bd list --title-contains '[operator-action]'` works.
- Case-insensitive client-side filter.
- Composable with all existing flags (--status, --priority,
  --json, etc.).
- New `apply_title_contains_filter()` helper adjusts both
  `data.beads` array and `data.count`.

## Diff summary

- Files touched (+35 / −~95 dedup):
  - `crates/caco-cli/src/lib.rs`:
    - `--title-contains` ArgSpec in BD_LIST_ARGS.
    - Filter application in `dispatch_bd_list` after
      `apply_worker_age_view`.
    - `apply_title_contains_filter()` (~25 LOC).
    - Removed duplicate bd-8fe920 CommandSpec, dispatch arm,
      and dispatch fn (main already has bd-020bc1's version).

## Verification

- `cargo build -p caco-cli`: clean.
- `cargo test-small`: 56 pass.
- `cargo clippy -p caco-cli --lib -- -D warnings`: clean.

## Operator-takeaway

```
$ caco bd list --status open --title-contains '[operator-action]'
```

Equivalent to `caco operator-actions list` but composable
with any other bd-list flags.
