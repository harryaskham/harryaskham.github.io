# Session summary — bd list --title-contains broken by daemon-side limit (bd-769a65)

## Goal

`caco bd list --title-contains 'Disk'` returned "no beads found"
even when bd-a167d6 with "Disk climb..." was clearly present.

## Bead(s)

- `bd-769a65` — caco bd list --title-contains returns 'no beads
  found' for clearly matching substrings (P2 bug, my regression
  from bd-f64f70).

## Root cause

The `--title-contains` filter is client-side (post-fetch). The
daemon's `--limit` (default 100) ran FIRST, returning the first
N beads in sort order. If none of the daemon's pre-trimmed N
contained the needle, the client-side filter found nothing and
emitted "no beads found" — even when matching beads existed
beyond the daemon's window.

## Before state

- `--limit` always passed through to daemon as-is.
- Title filter applied to whatever the daemon returned.
- Small `--limit` + `--title-contains` → false negatives.

## After state

- When `--title-contains` is set: request `limit=2000` from
  daemon (generous window) regardless of user `--limit`.
- After title filter, re-apply user's `--limit` via new
  `apply_post_filter_limit` helper that trims `data.beads`
  and updates `data.count`.
- 2 new tests:
  - `post_filter_limit_truncates_and_updates_count`
  - `post_filter_limit_no_op_when_already_smaller`

## Diff summary

- Files touched (+62 / −2):
  - `crates/caco-cli/src/lib.rs`: limit override + post-filter
    trim helper + 2 tests.

## Verification

- `cargo test -p caco-cli --lib title_contains`: 2 pass.
- `cargo test -p caco-cli --lib post_filter_limit`: 2 pass.
- `cargo clippy -p caco-cli --lib --tests -- -D warnings`: clean.
- Local repro:
  - Before: `caco bd list --title-contains Disk --limit 5` →
    "no beads found".
  - After (local build): returns bd-a167d6 correctly.

## Operator-takeaway

Closes a sharp regression in bd-f64f70's title filter UX. The
2000-row daemon window is enough for ~all real cacophony
projects; if a future project ever exceeds 2000 open beads, this
becomes a soft cap and we'd need server-side title filtering.
For now, server-side is overkill and client-side scales fine.
