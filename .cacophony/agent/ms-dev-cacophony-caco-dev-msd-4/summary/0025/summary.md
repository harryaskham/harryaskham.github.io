# Summary 0025 — bd-5705c2: client-side title length validation

## Bead
bd-5705c2 (P2, bug, claimed) — bd-ea2ddd action item 2:
"caco bd create: client-side reject titles >500 chars before
they hit JSONL." Filed by me as the work item; companion to
bd-3af67d (already-landed server-side poison-pill quarantine in
index_mutation_in_tx).

## Background
At ~04:53Z helsinki-cacophony-caco-ctrl filed bd-943e85 with
an 8998-char title — an entire `caco msg inbox` output had been
shell-substituted into `--title`. v1.2.490 daemons accepted it;
v1.2.491 enforces CHECK(length(title) BETWEEN 1 AND 500), so on
upgrade every wedged daemon's reimport_journal aborted on that
single mutation, taking the entire bd subsystem cluster-wide
offline (test-user-hel report this turn).

cluster-ctrl operator-rewrote the journal line in place
(8998 -> 88 chars), unwedging the local node. bd-3af67d landed
server-side poison-pill skip in index_mutation_in_tx so future
bad rows can't re-wedge. This bead closes action item 2:
prevent the bad input from being written in the first place.

## Change

`crates/caco-cli/src/lib.rs`:

- New `validate_bead_title_length(&str) -> Result<(), String>`
  helper. Checks `title.chars().count()` against `1..=500`
  (codepoint count matches sqlite's `length()` semantics on the
  column). On failure returns a friendly message naming the
  actual length, the cap, and the first 80 chars for context.
- `dispatch_bd_create` calls it immediately after resolving the
  title from `--title` / positional args, before constructing
  the request body.
- `dispatch_bd_update` calls it inside the `--title` branch so
  field updates can't re-poison either.
- Both wire the helper through `bd_cli_error(..., "invalid_argument", &msg)`
  so the failure surfaces as a structured CLI error in JSON
  mode and a clean text line in TTY mode.

### Tests

`crates/caco-cli/src/lib.rs::tests`:

- `validate_bead_title_length_accepts_in_range_and_rejects_out_of_range`:
  - 1-char, 250-char, 500-char all pass
  - empty: rejected with "must not be empty"
  - 501 (one over): rejected with both 501 and 500 in the msg
  - 8998 (the actual bd-943e85 inbox-dump shape): rejected with
    8998 in the msg AND a "First 80 chars" diagnostics preview
- `validate_bead_title_length_counts_codepoints_not_bytes`:
  250 emoji codepoints (1000 bytes) passes — we count the same
  way sqlite does, not by byte length.

## What this does NOT do

- Does not validate description length, label values, or any
  other field. Scoped to titles since that's the shape that
  caused the cluster-wide wedge. Other fields can grow their
  own validators when their failure modes manifest.
- Does not change the daemon-side validation (bd-3af67d already
  did that). Defense in depth: both layers reject independently.
- Does not warn on titles in 400..=500 range. The cap is hard
  at 500.

## Verification

- `cargo test -p caco-cli --lib validate_bead_title_length`: 2/2.
- `cargo test-small`: all green (~4256 tests).
- `cargo clippy --workspace --no-deps`: only the pre-existing
  unrelated `dispatch_operator_actions_list` dead-code warning.

## Operational impact

A future `caco-ctrl` agent that accidentally substitutes
`$(caco msg inbox)` (or any other multi-KB output) into
`--title` now gets:

  error: invalid_argument: title is 8998 chars; the bd subsystem
  caps titles at 500 chars (CHECK constraint). Truncate or
  reword. First 80 chars: "..."

Instead of silently filing the bead, having it propagate
through git-sync to every node, and wedging every v1.2.491+
daemon on the next restart.

The 80-char preview helps the operator see WHAT got
substituted (often the start of the actual intended title)
without dumping the full multi-KB blob into the error.

## Companion / related

- bd-3af67d (LANDED): server-side poison-pill quarantine in
  index_mutation_in_tx (skip-and-log instead of abort-tx).
- bd-ea2ddd action 1 (DONE by cluster-ctrl): in-place repair of
  bd-943e85 line 2596.
- bd-ea2ddd action 3 (open): investigate the shell-quoting
  accident upstream — out of scope here, separate workflow.
- bd-fb832d (open): operator effort estimation. Pairs as the
  "validate operator inputs at the CLI" cluster.

## Stale work discarded this turn

I had a parallel branch with my own version of the bd-3af67d
fix (skip in rebuild_index_from_journal only) plus the
bd-845653 reintegrate-footer fix. Hard-reset to origin/main
when I noticed bd-3af67d landed with a stronger inner-function
fix that protects ALL call sites, not just replay. bd-845653
work is gone too — would need to re-derive if I pick it back
up. (My summary 0024 from earlier this session DID land —
this is a separate piece of in-progress work that overlapped.)

## Next

Reintegrate, close bd-5705c2, idle. Then sweep my still-claimed
beads from earlier this session and close anything that
actually landed.
