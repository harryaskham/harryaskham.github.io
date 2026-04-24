# Session summary 0030 — bd-bc4d31 + bd-6be206 + bd-b6de3d (CLI dry-run / note triad)

## Goal
Close three CLI affordance gaps in `caco bd`:
1. bd-bc4d31 — `bd create` lacks `--dry-run` / `--preview` (CLI parity with TUI bd-150104).
2. bd-6be206 — `bd close` lacks user-facing closure-rationale flag (`--reason` exists but reads as admin-only).
3. bd-b6de3d — `bd update --title 'foo'` silently overwrites with no preview/diff (bd-4c8fdd Issue 5; same footgun-class as the bd-9969d1 / bd-ee2dd4 broken-on-main wave today).

## Bead(s)
- **bd-bc4d31 CLOSED** — `--dry-run` / `--preview` short-circuits POST in `dispatch_bd_create`. JSON envelope echoes `would_create` body. 11/11 bd_create tests pass; live verified zero beads created. Landed: commit e21dd87f3.
- **bd-6be206 CLOSED** — `--note` / `--message` aliases for `--reason`. Coalesce precedence `--reason > --note > --message`; bd-9119a2 admin-override contract unchanged. Pin test on BD_CLOSE_ARGS. Self-tested by using `--note` on its own closure. Landed: commit ee2c6a2b1.
- **bd-b6de3d (this commit)** — `--dry-run` / `--preview` short-circuits PATCH in `dispatch_bd_update`; fetches current bead via GET, renders `current → proposed` diff per touched field. Long values truncated to 80 chars + char-count footnote; `(no change)` tag for identical fields. JSON envelope includes both `current` (full bead) and `would_update` (the body). Pin tests on flags + canonical 'No update was sent' marker. 11/11 bd_update tests pass. Live-verified zero mutation. Held reintegrate per caco-ctrl directive while bd-ee2dd4 (P0 broken-on-main) was red; rebased + reintegrating now that main is green.
- bd-ee2dd4 broken-on-main observed mid-session (msm-2 fixed it).

## Diff summary
- `crates/caco-cli/src/lib.rs`:
  - BD_CREATE_ARGS gains `--dry-run`, `--preview` (bd-bc4d31).
  - BD_CLOSE_ARGS gains `--note`, `--message` (bd-6be206).
  - BD_UPDATE_ARGS gains `--dry-run`, `--preview` (bd-b6de3d).
  - `dispatch_bd_create`: short-circuit after body assembly + client validation, before HTTP POST. Renders human + JSON dry-run forms.
  - `dispatch_bd_close`: coalesce `--reason | --note | --message` into `admin_reason` slot.
  - `dispatch_bd_update`: short-circuit after body assembly + no-op check, GET current state, render current→proposed diff (human + JSON).
  - 5 new tests: `bd_create_args_include_dry_run_and_preview`, `bd_create_dry_run_text_output_contains_no_creation_marker`, `bd_close_args_include_note_message_and_reason`, `bd_update_args_include_dry_run_and_preview`, `bd_update_dry_run_text_output_contains_no_update_marker`.
- New summary file: this file.
- Total: 1 source file changed, ~330 insertions across the three commits.

## Operator-takeaway
- All three state-mutating `bd` verbs (create / update / close) now have either a dry-run (create + update) or a discoverable rationale flag (close). The bd-b6de3d retitle-footgun is now preventable with `--dry-run`.
- Pin tests on BD_*_ARGS lock the new flags into the dispatcher's strict-flag-refusal so future spec edits can't silently regress them.
- Pattern: dry-run short-circuits AFTER client validation, BEFORE HTTP — same errors a real call would yield, just no network mutation. JSON envelope `would_create` / `would_update` matches the EXACT body the dispatcher would have sent.
- Holding reintegrate when caco-ctrl flags `[broken-on-main]` is now my default.

## Before state
`bd create` / `bd close` / `bd update` lacked the listed affordances; bd-4c8fdd Issue 5 footgun live; bd-ee2dd4 red on main mid-session.

## After state
- bd-bc4d31, bd-6be206 closed on main.
- bd-b6de3d landing in this reintegrate.
- Session tally: 17 closed/landed + 3 reopened-with-notes + 6 follow-up beads filed.
- Next summary index: 0031.
