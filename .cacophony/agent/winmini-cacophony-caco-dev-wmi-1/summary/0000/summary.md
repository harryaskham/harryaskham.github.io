# Session summary — bd-9c55aa Issue 7: caco msg inbox --kind/--type alias conflict

## Goal

Reject `caco msg inbox --kind X --type Y` (with conflicting values)
explicitly instead of silently ANDing the alias pair into a guaranteed-
empty result. Closes Issue 7 of the bd-9c55aa multi-issue ticket.

## Bead(s)

- `bd-9c55aa` — caco msg inbox MASTERCLASS validator surface (P3 bug,
  multi-issue). This session pins **Issue 7 only** (alias conflict).
  Issues 1-4 are POSITIVES (no code change needed); Issue 5 (`--max-age
  -1` flag-parser ambiguity) is a parser-level concern with cluster-
  wide impact and stays in the bead body for future work; Issue 6
  (`--grep ''` empty-string semantics) is a documented convention
  question; Issue 8 (text phrasing) is cosmetic.

## Before state

```
$ caco msg inbox --project cacophony --kind speak --type broadcast
[stdout: "no unread messages"]
[exit: 0]
```

The `--kind` / `--type` flags are documented aliases (per --help and
the bd-60c7de shared validator). Supplying both with conflicting
values silently ANDed them via the dispatcher's `or_else` chain
(which actually picked `--kind` — but the operator's intent was
ambiguous). Result was always empty.

## After state

```
$ caco msg inbox --project cacophony --kind speak --type broadcast
error: --kind speak and --type broadcast are aliases for the same filter; pass only one (or use the same value for both)
[exit: 1]
```

Same-value (`--kind direct --type direct`) still works — only
**conflicting** combinations are rejected. This matches the existing
`--tail`/`--limit` warning pattern right above the new check.

## Diff summary

- 1 file changed, +13 / -0 (`crates/caco-cli/src/lib.rs` msg inbox
  dispatch).

## Validation

- `cargo check -p caco-cli`: clean.

## Operator-takeaway

Operators relying on `--kind` and `--type` as separate filters
discover immediately that they're aliases instead of getting a
silent-empty result. Same-value passes through unchanged so any
existing scripts that double-pass the same value (defensive
duplication) continue to work.
