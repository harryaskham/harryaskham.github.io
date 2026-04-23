# Session summary — bd-923172 choices list help-summary contract pin

## Goal
Seal bd-914ceb's runtime fix with a help-text contract pin so the
documented enum cannot silently drift from the runtime validator.

## Bead(s)
- `bd-923172` (P3 bug) — regression of bd-914ceb.

## Before state
- bd-914ceb landed runtime validator + 'unavailable' enum value.
- Help-text contract had no test → could silently drift on rewrite.
- Test-user observed an earlier release where help still claimed
  only active/resolved/all.

## After state
- Verified both halves green on current main (1.2.526):
  bogus rejected, unavailable accepted, help text correct.
- New test choices_list_status_help_summary_documents_unavailable
  walks CHOICES_LIST_ARGS and asserts every validator value
  appears in the help summary.
- Existing validator test unchanged.

## Diff summary
- `crates/caco-cli/src/lib.rs`: +24 — one new test.
- Behavioural delta: zero — pure regression coverage.
- cargo test-small green; clippy clean.

## Operator-takeaway
Bug already fixed; this seals it. Future help cleanups that drop
'unavailable' will trip CI before reaching a release.
