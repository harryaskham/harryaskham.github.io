# Session summary — caco-tui broken-on-main fixture backfill (bd-bce6ea)

## Goal

P1 broken-on-main: `cargo test -p caco-tui --lib` failed to
compile after bd-87f5bf+follow-ups added `tmux_history_limit` and
`tmux_history_size` fields to `state::AgentDisplayState` and
`ui_stream::AgentSnapshot`. ~95 E0063 missing-field sites across
the caco-tui crate plus 20 E0560 misplaced-field sites left by a
peer's partial fix.

## Bead(s)

- `bd-bce6ea` — [broken-on-main] caco-tui lib tests: 95 sites
  missing tmux_history_limit + tmux_history_size

## Before state

- `cargo test -p caco-tui --lib` failed to compile.
- 75 E0063 errors (missing fields in valid literal sites).
- 20 E0560 errors (peer wmi-2's bd-bf1e86 cycle wrongly inserted
  the new fields into 10 *nested* AttachMetadata / SessionKicked
  Modal literal positions where the fields don't belong).
- 2818 caco-tui lib tests untouchable.

## After state

- `cargo test -p caco-tui --lib`: 2818 passed; 0 failed.
- Every AgentDisplayState / AgentSnapshot literal now carries
  `tmux_history_limit: None, tmux_history_size: None` matching
  the bd-87f5bf default-None semantics for non-tmux paths.

## Diff summary

- Files touched (75 inserts + 20 deletes, net +130):
  - `crates/caco-tui/src/state/tests.rs` (66 fixture sites)
  - `crates/caco-tui/src/shell_cwd.rs` (2)
  - `crates/caco-tui/src/shell_tile_lane.rs` (2)
  - `crates/caco-tui/src/views/agent_detail.rs` (1)
  - `crates/caco-tui/src/views/chat.rs` (1)
  - `crates/caco-tui/src/views/fuzzy_picker.rs` (1)
  - `crates/caco-tui/src/views/project_tree.rs` (2)
  - `crates/caco-tui/src/app.rs` (10 misplaced-field deletes)

## Approach

Mechanical, three passes over compiler output:

1. Parse `cargo test -p caco-tui --lib --no-run` stderr to extract
   `(file, line, struct_name)` from every E0063 site.
2. Filter to struct_name ∈ {AgentDisplayState, AgentSnapshot} so
   we don't poison nested struct literals (the trap wmi-2 hit).
3. For each filtered site, walk forward from the opening line
   tracking `{` / `}` depth, find the matching outer closing
   brace, insert the two field-None lines immediately before it
   using the previous field's indent. Apply edits in reverse line
   order so earlier sites don't shift.
4. Then parse E0560 output; delete the 10 misplaced lines that
   wmi-2's earlier partial fix left in AttachMetadata /
   SessionKickedModal literals.

## Embedded artefacts

(none — pure fixture backfill; no behaviour change)

## Operator-takeaway

`cargo test -p caco-tui --lib` is unwedged for everyone. Future
follow-ups that add fields to widely-instantiated types should
either (a) add `Default` and use `..Default::default()` at the
literal sites, or (b) bulk-update fixtures in the same commit
that adds the field. The trap that bit wmi-2 — using a regex to
match `AgentDisplayState {` literally and then inserting before
the *first* `}` — is the same one this fix avoided by tracking
brace depth properly.
