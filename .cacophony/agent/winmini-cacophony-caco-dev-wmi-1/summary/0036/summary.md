# Session summary — caco scratch disconnect honesty + project label (bd-2b10dd)

## Goal

`caco scratch disconnect` reported "Disconnected: true" even
when no such connection existed. `caco scratch show` rendered
an empty "Project:" line for global notes.

## Bead(s)

- `bd-2b10dd` — caco scratch UX bugs (P3 bug, 3 issues filed)

## Before state

- `caco scratch disconnect` always rendered "Disconnected: true"
  (hardcoded), even for nonexistent connections.
- `caco scratch show` rendered an empty "Project:" line for
  global notes (no project context).

## After state

- `caco scratch disconnect` reads daemon's actual `disconnected`
  field and renders "true" or "false (no such connection)".
- `caco scratch show` renders "Project: global" when project is
  empty.

## Issues addressed

### Issue 2: disconnect lies (FIXED)

- CLI hardcoded "Disconnected: true" instead of reading the
  daemon's actual `disconnected` boolean.
- Daemon already returns `disconnected: false` when DELETE
  matched 0 rows; CLI was ignoring it.
- Now reads `result["data"]["disconnected"]` and renders
  either "true" or "false (no such connection)".

### Issue 3: empty Project label (FIXED)

- `caco scratch show` rendered "Project:" with empty value
  for global notes.
- Now renders "Project: global" when project string is empty.

### Issue 1: append upsert semantics (DEFERRED)

- Per SPEC §19.6.1, `append` is documented to create-on-absent
  ("append creates the note when absent"). This is intentional
  upsert behavior. Filed bead asks for `--create=false`
  opt-out which is a feature, not a bug.
- Not addressed in this patch; would need a separate small
  feature bead if operators want stricter semantics.

## Diff summary

- Files touched (+15 / −5):
  - `crates/caco-cli/src/lib.rs`: dispatch_scratch_disconnect
    + dispatch_scratch_show.

## Verification

- `cargo build -p caco-cli`: OK.
- `cargo clippy -p caco-cli --lib --tests -- -D warnings`: clean.
- Behavioral tests deferred (would need daemon round-trip;
  the daemon-side `disconnect_note` already returns the right
  bool — verified by reading scratchpad.rs).

## Operator-takeaway

`caco scratch disconnect` no longer lies. Operators can now
trust the output. Global notes display "global" instead of
an unhelpful empty value.
