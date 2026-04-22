# Session summary — caco choices: empty array + out-of-bounds rejection (bd-b5c18d)

## Goal

`caco choices present --choices '[]'` accepted an empty array
and let `choices resolve --selected-index 0` succeed against it,
silently falling through to a freeform resolution even when
freeform was disallowed.

## Bead(s)

- `bd-b5c18d` — caco choices present accepts empty --choices and
  lets resolve succeed (P3 bug)

## Before state

- CLI: empty `--choices '[]'` was accepted and forwarded to
  daemon → unresolvable choice created.
- Daemon: `selected_index` was used as `choices.get(idx)` →
  silently None for any out-of-bounds index → resolution
  succeeded with no selected_label, no error.

## After state

- CLI: `dispatch_choices_present` rejects empty array
  client-side (unless `--allow-freeform` is set).
- Daemon: `handle_resolve_choice` bounds-checks
  `selected_index` against `resolved.choices.len()`. If OOB,
  returns the same envelope shape as the no-active-choice arm
  with `resolved: false` + clear error mentioning bd-b5c18d
  and recommending `--freeform-text`.
- 1 daemon test:
  `choices_resolve_rejects_out_of_bounds_selected_index`
  presents a 1-option choice, tries to resolve with
  selected_index=5, asserts envelope has resolved=false and
  the error message.

## Diff summary

- Files touched (+78 / 0):
  - `crates/caco-cli/src/lib.rs`: empty-array guard in
    dispatch_choices_present.
  - `crates/caco-daemon/src/choices.rs`: bounds-check guard
    at top of handle_resolve_choice's Some arm.
  - `crates/caco-daemon/src/lib.rs`: 1 integration test.

## Verification

- `cargo test -p caco-daemon --lib choices`: 17 pass (was 16).
- `cargo clippy -p caco-cli --lib --tests -- -D warnings`: clean.
- `cargo clippy -p caco-daemon --lib --tests -- -D warnings`: clean.

## Operator-takeaway

Closes a sharp UX bug: callers can no longer create
unresolvable choices, and out-of-bounds resolutions fail
loudly instead of silently. The error message points to
`--freeform-text` as the correct escape hatch.

Sub-issue noted in bd-b5c18d ("caco choices show doesn't
display option list for active choices") deferred — could be
a separate bead if substantive.
