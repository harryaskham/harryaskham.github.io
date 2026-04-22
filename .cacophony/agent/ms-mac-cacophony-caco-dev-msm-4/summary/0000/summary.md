# bd-93e798 — bd create title-too-long error names $() and --description

## Goal
Make the client-side title-length rejection actually tell the
operator what to do, since bd-943e85's 8998-char poison-pill was
filed by an accidental shell substitution and the prior wording
gave no hint that was the cause.

## Bead(s)
- bd-93e798 (P2 bug). Acceptance items 1 (CLI bd create), 2 (CLI
  bd update --title), 4 (MCP boundary). Item 3 (test) covered by
  the new + existing unit tests.

## Before state
- `crates/caco-cli/src/lib.rs::validate_bead_title_length`
  already enforced 1..=500 chars and was called from both
  `dispatch_bd_create` (line 24114) and `dispatch_bd_update`'s
  `--title` branch (line 23704). MCP `caco_bd_create` /
  `caco_bd_update` route through the same dispatchers via
  `invocation_segments`, so the validator was reached there too.
- Error wording named the consequence ("caps titles at 500 chars
  (CHECK constraint). Truncate or reword.") but not the cause.
  Operators kept hitting it via stray $() substitution into
  --title and had no diagnostic pointer.

## After state
- `validate_bead_title_length` returns the bead-mandated wording:
  `"title too long (N chars, max 500). Did you accidentally pass
  a shell $() substitution? Move long content to --description.
  (CHECK constraint enforces 500 server-side.) First 80 chars:
  ..."`. Leads with operator-actionable phrasing, names the
  most common cause, points at the right escape hatch, and keeps
  the 80-char preview that operators use to recognise escaped
  command output at a glance.
- Coverage of all four acceptance items unchanged structurally;
  this is a UX-only fix on top of the existing gate.

## Diff summary
- `crates/caco-cli/src/lib.rs` (+32/-2):
  - Re-wording inside `validate_bead_title_length` body, with
    inline bd-93e798 doc comment explaining the diagnostic intent.
  - New unit test
    `validate_bead_title_length_error_mentions_shell_substitution_and_description`
    asserts `$()`, `--description`, and `too long` are all present.
  - Existing `_accepts_in_range_and_rejects_out_of_range` and
    `_counts_codepoints_not_bytes` tests still pass unchanged.

## Tests
- `cargo build -p caco-cli` — clean.
- `cargo clippy -p caco-cli --all-targets -- -D warnings` — clean.
- `cargo test -p caco-cli --lib validate_bead_title_length` — 3/3
  pass (1 new + 2 pre-existing).

## Operator-takeaway
Next time a worker accidentally runs
`caco bd create --title "$(caco msg inbox)"` the error tells them
what happened and where to put the long content, instead of
making them re-derive the cause from a generic "too long" notice.
The actual 500-char cap (server-side CHECK + client-side
validator) is unchanged.
