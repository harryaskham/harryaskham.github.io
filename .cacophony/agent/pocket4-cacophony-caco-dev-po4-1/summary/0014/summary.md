# Session summary — bd-6c5106 reject empty --note-id on scratch writes

## Goal

Close a sister-of-bd-2b10dd / bd-89df3d data-integrity hole where
`caco scratch append --note-id '' --text foo` silently materialised
an orphan note that no read path could access, because the show
surface (bd-89df3d) already rejected empty IDs but the write
surfaces did not. Symmetrise read/write rejection of empty IDs.

## Bead(s)

- `bd-6c5106` — caco scratch append accepts --note-id '' (empty string) and silently creates an orphan note inaccessible to caco scratch show (P3 bug)

## Before state

- `dispatch_scratch_show_impl` correctly rejected empty `--note-id`
  with the actionable message `--note-id cannot be empty (note IDs
  must be non-empty strings; see 'caco scratch list' for available
  notes)` (bd-89df3d).
- `dispatch_scratch_write_impl`, `dispatch_scratch_append_impl`,
  `dispatch_scratch_connect_impl`, `dispatch_scratch_disconnect_impl`
  all accepted empty `--note-id` and forwarded it to the daemon.
- For append in particular, `ensure_scratch_note` PUT'd a note keyed
  on the empty string and the subsequent append succeeded with
  `Appended: 3 bytes` despite the note being unreachable through any
  read path. The result was a data-integrity hole: bytes the operator
  thought they were writing went somewhere unreadable.

## After state

- New shared helper `reject_empty_note_id(note_id, json_requested)`
  at the top of `crates/caco-cli/src/scratch_cmd.rs` returns the
  canonical empty-id error in both human (`Err(CliError)`) and
  JSON (`Ok(Outcome { exit_code: 1, ... })`) shapes.
- Wired into all four write-surface dispatchers, each with a one-liner
  guard immediately after the existing `--note-id is required` check.
- Refactored `dispatch_scratch_show_impl`'s existing inline empty-id
  guard to call the same helper, keeping the show / write paths
  bit-identical in their empty-ID handling.

## Diff summary

- Commit: 3075c800c
- Files touched:
  - `crates/caco-cli/src/scratch_cmd.rs` — new helper +
    4 write-surface guards + DRY refactor of show-surface guard
  - `crates/caco-cli/src/lib.rs` — new test
    `scratch_write_paths_reject_empty_note_id_bd_6c5106`
- Tests: cargo test-small 261/261 pass; new test passes covering all
  four write surfaces (append/write/connect/disconnect) with
  `--note-id ''` + the other required flags.

## Operator-takeaway

Read/write surfaces are now symmetric on empty `--note-id`: the
operator gets the same actionable error from `caco scratch <op>` no
matter which surface they hit. The bug was structurally identical
to bd-89df3d / bd-2b10dd; this PR closes the asymmetric write/read
hole for the four remaining surfaces. Non-empty IDs and the daemon
side are unchanged.
