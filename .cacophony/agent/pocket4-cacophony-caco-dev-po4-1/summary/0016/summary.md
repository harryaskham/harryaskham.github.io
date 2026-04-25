# Session summary — bd-4134f7 reject empty/whitespace --title on notify send

## Goal

Close a within-namespace empty-string drift on `caco notify send`:
the missing-arg path correctly errored `--title is required` but
`--title ''` and `--title '   '` bypassed validation and silently
created a notification with an empty/whitespace-only title.

## Bead(s)

- `bd-4134f7` — caco notify send accepts --title '' and --title '   ' (P3 bug, sister of bd-287646)

## Before state

- The `notify send` dispatch arm extracted `--title` via
  `flags.get("--title").ok_or_else(...)` only — i.e. it caught the
  absent-flag case but accepted any non-None value, including empty
  and whitespace-only strings.
- A whitespace title was unidentifiable in `caco notify list` output
  unless the operator inspected each notification body.

## After state

- New trim-check guard immediately after the missing-arg check:
  if `title.trim().is_empty()` → `CliError`:
  `--title value cannot be empty for notify send (bd-4134f7)`.
- Error wording matches the gold-standard `value cannot be empty`
  pattern from the bd-2caf68 / bd-287646 / bd-6c5106 sisters.

## Diff summary

- Commit: 267125cdd
- Files touched: `crates/caco-cli/src/lib.rs` (+40)
- New source-level guard test
  `notify_send_rejects_empty_or_whitespace_title_bd_4134f7`
  asserts (a) `title.trim().is_empty()` is checked in the dispatch
  arm and (b) the canonical wording is used.
- Tests: cargo test-small 261/261 pass; new test passes.

## Operator-takeaway

Adds the 7th–8th surface-pair (notify send + the broader empty-string
hygiene family) to the gold-standard required-string validation. The
behavioural delta is strictly tightening — non-empty / non-whitespace
titles continue to work unchanged.
