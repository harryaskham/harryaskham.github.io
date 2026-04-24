# Session summary — bd-d97b37: caco release namespace minor-drift fixes

## Goal

Pin the 3 minor drifts from the bd-d97b37 STRONG-POSITIVE sweep
of the caco release namespace (the 7 PROMOTE candidates remain
in body for the gold-standard-promote workstream):

- **Drift A**: `--limit ''` formatting double-space ('invalid
  --limit value:  (expected a positive integer)') — should
  short-circuit at empty check (bd-c3c0a0 canonical).
- **Drift B**: `release status --id ''` WITHIN-SURFACE
  inconsistency: HTTP-404-leak 'EOF while parsing a value at
  line 1 column 0' vs `--id bogus` which is clean structured.
- **Drift C** (related): `release logs --id ''` same HTTP-404-
  leak pattern; `release logs --id bogus` lacks the
  discoverability hint that `release status` already has.

Drift D (release list --foo bogus) is bd-b76723 cohort, not
unique. Not pinned.

## Bead(s)

- `bd-d97b37` — caco release namespace (P3, test-user). 7
  PROMOTE candidates preserved in body for the gold-standard
  retrofit workstream.

## Before state

```
$ caco release list --limit ''
error: invalid --limit value:  (expected a positive integer)
                              ^^ DOUBLE SPACE drift

$ caco release status --id ''
error: ... EOF while parsing a value at line 1 column 0
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ raw HTTP-404 leak

$ caco release logs --id ''
error: ... EOF while parsing a value at line 1 column 0
                                                  ^^^ same leak
```

## After state

```
$ caco release list --limit ''
error: --limit value cannot be empty (expected a positive integer; omit --limit for the default)

$ caco release status --id ''
error: --id value cannot be empty for release status. Run `caco release list` to see queued/active jobs.

$ caco release logs --id ''
error: --id value cannot be empty for release logs. Run `caco release list` to see queued/active jobs.
```

## Diff summary

- 1 file changed, +25 / -1 (`crates/caco-cli/src/lib.rs`):
  - `dispatch_release_status`: `--id ''` upfront guard + carry
    discoverability hint forward.
  - `dispatch_release_logs`: `--id ''` upfront guard + add the
    discoverability hint missing from this surface.
  - `validate_positive_limit` helper: short-circuit
    empty/whitespace at the front to fix the double-space drift
    fleet-wide. **25+ call sites benefit from this single fix**
    (release list, build list, test list, msg list, scratch
    list, and all other validate_positive_limit consumers).

## Validation

- `cargo check -p caco-cli`: clean.
- Existing `validate_positive_limit_rejects_zero_for_lines` +
  `validate_positive_limit_accepts_positive_lines` tests
  unaffected (they use '0' and '1'/'50', not empty string).

## Operator-takeaway

Within-surface inconsistency in caco release namespace fixed:
empty-string --id no longer leaks raw HTTP-404 + serde_json EOF.
Both release status + release logs now get the discoverability
hint pointing to `caco release list`. Bonus: the
validate_positive_limit helper update fixes the `--limit ''`
double-space drift across 25+ call sites in one shot.

7 PROMOTE candidates from bd-d97b37 remain in the body for
gold-standard-promote workstream:
- NOVEL required-flag with embedded discoverability pointer
  (8th variant).
- NOVEL inline-allowed-values 'Configured:' phrasing.
- GOLD-STANDARD security-WHY --project bogus.
- NOVEL --limit -1 clean (25th surface).
- Canonical structured-error envelope.
- DISCOVERABILITY HINT in error path.
- bd-1c1d0f USAGE-GUIDANCE adopted as-is.

Push-discipline (post-clarification): own-branch push allowed;
default-branch force-push banned; only reintegrate / complete
land work on main. This session continues to use only local refs
+ daemon-mediated reintegrate.
