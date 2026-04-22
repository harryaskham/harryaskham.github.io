# Session summary — caco bd update shell-quoting-safe description sources (bd-8c89a8)

## Goal

Make `caco bd update` description bodies safe to pass without shell-quoting hazards (backticks, `$VAR`, sed metacharacters), so operators don't silently blank a bead's description while trying to append to it.

## Bead(s)

- `bd-8c89a8` — caco bd update --description sed-piped: shell-quoting hazard with backticks/dollar-signs in description bodies. Filed by caco-doctor after corrupting bd-180c6d's description tonight; claimed and fixed.

## Before state

- Only `--description "$(cmd)"` was available. The literal `$` in `sed -n '/description:/,$p'` got mangled by outer shell quoting → daemon PATCH succeeded with a corrupt body → bd-180c6d description blanked. Real data-loss-shape risk.

## After state

Three new flags (`crates/caco-cli/src/lib.rs`):

- `--description-file PATH` — reads body from file verbatim (use `-` for stdin)
- `--description-stdin` — alias for `--description-file -`
- `--append-description` — appends instead of replaces; works with any of `--description` / `--description-file` / `--description-stdin`

Helpers added:
- `read_description_source(path)` — verbatim read from file or stdin
- `format_appended_description(existing, appended)` — markdown-clean concat with exactly one blank-line separator (handles empty sides, trailing/leading newline trim)
- `fetch_bead_description(...)` — GET current body for append mode

Validation rules (rejected at CLI before any PATCH):
- At most one of `--description` / `--description-file` / `--description-stdin`
- `--append-description` requires one of the three above (no-op guard)
- `--duplicate-of` cross-check now lists the new flags as conflicting
- The "no field flags" error message lists `--description-file` / `--description-stdin` so operators discover them

## Tests

7 new (caco-cli):
- `format_appended_description_inserts_blank_line_separator`
- `format_appended_description_handles_existing_trailing_newlines`
- `format_appended_description_handles_empty_existing`
- `format_appended_description_handles_empty_appended`
- `format_appended_description_strips_leading_newline_on_appended`
- `read_description_source_reads_file_verbatim` — round-trips a body full of backticks, `$VAR`, and sed metacharacters
- `read_description_source_returns_error_for_missing_file`

## Drive-by upstream-merge salvage

- bd-d8fc57 added `parent_bead_id: Option<String>` to `Bead` and `CreateBeadParams`. Inserted `parent_bead_id: None,` (or `params.parent_bead_id`) at ~270 literal-construction sites across `caco-beads`, `caco-daemon`, `caco-tui`. Did NOT add it to `SnapshotBead` / `BeadDisplayState` / `ui_stream::BeadSnapshot` / `AuditBeadRequest` / `BeadUpdate` / `event::ActionResult` variants which don't carry the field.
- bd-1d302a added `disable_hooks: Option<...>` to `Profile`. De-duplicated my prior fix once upstream landed its own.
- `dispatch_agent_logs` gained a `since: Option<&str>` parameter; updated three test call sites.
- `crates/caco-daemon/src/ui_stream.rs`: removed two more pairs of duplicate `tmux_history_limit/size` in test fixtures (companion's bd-ae6b7b sweep keeps re-adding them).

## Verification

- `cargo test -p caco-cli --lib format_appended_description read_description_source` — 7 / 0
- `cargo test-small` — all green (209 / 109 / 739 / 295 / 18 / 2817 / 56)
- `cargo check --workspace --tests` — clean (1 unrelated `too_many_arguments` clippy warn pre-dates this change)

## Diff summary

- Commit: `39c5e98d`
- 10 files changed, 378 insertions(+), 21 deletions(-)
- Most lines are the workspace-wide `parent_bead_id` salvage; the focused fix is ~120 lines in `crates/caco-cli/src/lib.rs`

## Out of scope

- `caco bd update --description-from-bead bd-XXX` (copy from another bead) — listed in the bead's "fix paths" but lower priority once stdin/file land
- Server-side echo of the resulting description so operators can confirm body content from the response (the existing `format_bead_detail` already shows it)

## Operator-takeaway

Pipe untrusted text safely:
```
echo "$(weird body with \`backticks\` and \$vars and sed -n '/x/,$p')" | caco bd update --bead-id bd-XXX --description-stdin
```
or
```
caco bd update --bead-id bd-XXX --description-file ./body.md --append-description
```
