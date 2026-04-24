# Session summary — bd-9d3623 Issues 5+7: msg history --type validator + msg stats limit phrasing

## Goal

Fix two sister-surface drifts inside the msg namespace:
- **Issue 5**: `caco msg history --type bogus` silently accepted
  invalid values (sister surface to `caco msg inbox` which validates
  via bd-60c7de). Operator who learned the gold-standard error
  message from inbox naturally retried on history and got 0 results
  with no signal that the filter had been ignored.
- **Issue 7**: `caco msg stats --limit 0` / `--top 0` returned the
  minimal "must be > 0" message, drifting from the gold-standard
  inbox phrasing that names alternatives sister-symmetrically.

## Bead(s)

- `bd-9d3623` — caco msg stats / snapshot / history sweep (P3 bug,
  multi-issue). This session pins Issues 5 and 7. Issues 1-4 are
  POSITIVES (cohort observations, no code change). Issue 6 (`--top
  -1` / `--max-age -1` parser ambiguity) is the cross-cutting
  parser concern from bd-9c55aa Issue 5 / bd-754fde Issue 6 and
  remains for a parser-level fix. Issue 8 is cosmetic.

## Before state

```
$ caco msg history --project cacophony --type bogus
caco msg history (bd-d4e93d): project=cacophony returned=0 (cap=100)
[exit 0]                          # silently dropped the invalid filter

$ caco msg stats --project cacophony --limit 0
error: --limit must be > 0        # minimal phrasing

$ caco msg stats --project cacophony --top 0
error: --top must be > 0          # minimal phrasing
```

## After state

```
$ caco msg history --project cacophony --type bogus
error: unknown --type value 'bogus'. Allowed: direct, broadcast, speak, system

$ caco msg stats --project cacophony --limit 0
error: --limit must be >= 1 (use --limit 1 for a single result, or omit --limit for the default)

$ caco msg stats --project cacophony --top 0
error: --top must be >= 1 (use --top 1 for a single result, or omit --top for the default)
```

`msg history --type` now validates against the same `[direct,
broadcast, speak, system]` enum as `msg inbox --kind/--type` (using
the existing `validate_enum_flag` helper). `msg stats --limit` and
`--top` zero-rejection messages now match the gold-standard inbox
phrasing.

## Diff summary

- 1 file changed, +20 / -3 (`crates/caco-cli/src/lib.rs` — msg
  history dispatch + dispatch_msg_stats limit/top guards).

## Validation

- `cargo check -p caco-cli`: clean.

## Operator-takeaway

Sister-surface symmetry restored within the msg namespace: scripts
or operators using `--type` filters on `msg history` now get the
same validator behaviour as `msg inbox`, and `msg stats` zero-input
errors now name the alternatives sister-symmetrically with the rest
of the cluster's `--limit` / `--top` guards.

The novel Issue 1 gold-standard "explain-model-and-suggest-fix"
range-conflict error message remains as a positive cohort exemplar
for any future range-validator work (e.g. `--priority-min /
--priority-max`).
