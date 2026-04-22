# Session summary — bd-c36993 unblock caco-tui + caco-daemon test compile

## Goal

Workspace `cargo test-small` was broken on main (~85 E0063
"missing fields" errors in caco-tui plus 6 E0062 duplicate-field
errors in caco-daemon) after bd-7ef076 / bd-b69cf3 added
`tmux_history_limit` + `tmux_history_size` to several display
structs without sweeping the call sites. Restore green so peers
can keep landing.

## Bead(s)

- `bd-c36993` — [broken-on-main] caco-tui lib tests fail to
  compile: `AgentDisplayState` + `SessionKickedModal` need
  `tmux_history_limit/size` on ~95 struct-init sites. Filed by me
  this session because msd-1 had spotted it but not yet filed; ~85
  caco-tui sites + 6 caco-daemon sites confirmed.

## Before state

- `cargo build -p caco-tui --tests`: 85 errors. Mix of E0063
  (missing fields on `AgentDisplayState`, `AgentSnapshot`) and
  E0560 (no field named — call sites set fields on
  `AttachMetadata` / `SessionKickedModal` whose definitions
  hadn't yet been extended).
- `cargo build -p caco-daemon --tests`: 6 E0062 duplicate-field
  errors in `crates/caco-daemon/src/ui_stream.rs` from a
  rebase artifact where `tmux_history_limit` + `tmux_history_size`
  appeared twice in the same struct literal.
- `cargo test-small`: blocked workspace-wide.

## After state

- `cargo test-small`: PASS 209 + 109 + 739 + 291 + 18 + 2818 + 56
  green.
- `cargo clippy --workspace --all-targets -- -D warnings`: clean.
- `AttachMetadata` and `SessionKickedModal` struct definitions
  extended to include the two fields (with bd-b69cf3 / bd-c36993
  doc comments), matching what call sites already supplied.
- 84 caco-tui struct-literal sites now include
  `tmux_history_limit: None, tmux_history_size: None,` immediately
  before their closing brace at sibling indentation.
- 6 duplicate field assignments in `ui_stream.rs` removed (the
  trailing duplicates; earlier in-order assignments retained).

## Files touched

- `crates/caco-tui/src/state/mod.rs` (+13 / -0): two struct
  definition extensions + one call-site fix.
- `crates/caco-tui/src/state/tests.rs` (+138 / -0): 69 sites.
- `crates/caco-tui/src/views/{agent_detail,chat,fuzzy_picker,
  project_tree}.rs` (+10 / -0): 5 sites.
- `crates/caco-tui/src/{shell_cwd,shell_tile_lane}.rs` (+16 / -0):
  8 sites.
- `crates/caco-tui/src/{app,client}.rs` (+4 / -0): 2 sites.
- `crates/caco-daemon/src/ui_stream.rs` (+0 / -6): duplicate
  removal.

Total: 11 files, +181 / -6.

## Diff summary

Single mechanical sweep done via a Python script that:

1. Parsed `cargo build -p caco-tui --tests` E0063 errors, extracted
   the (path, line, col) of each missing-fields struct literal.
2. For each site, opened the file, scanned forward from (line, col)
   to find the opening `{`, then brace-walked (string/char/comment
   aware) to find the matching `}`.
3. Measured sibling indentation from the first non-blank field
   line, normalized any missing trailing comma on the preceding
   field, then inserted the two `None` lines just before the
   closing brace.
4. Recompiled. One iteration was sufficient: the 85 sites all
   resolved cleanly.

For `AttachMetadata` and `SessionKickedModal` struct extensions:
the call sites already supplied the new fields (msm-5's bd-b69cf3
WIP), so the struct definitions caught up to match. Each gains
two `pub field: Option<u32>` declarations with a doc comment
naming bd-b69cf3 (parent feature) and bd-c36993 (this commit).

For `ui_stream.rs`: a `python3` one-liner deleted the 6 duplicate
field-assignment lines (3417, 3418, 3463, 3464, 5082, 5083) in
descending order so indices stayed stable. The earlier in-order
assignments (alongside other `tmux_*` fields) were kept; the
trailing rebase-artifact duplicates were removed.

## Operator-takeaway

`cargo test-small` is unblocked workspace-wide. Other agents who
need to validate before reintegrate are no longer blocked by the
bd-7ef076 / bd-b69cf3 in-flight WIP. msm-5 still owns the
actual UI render slice (bd-b69cf3) — this commit just makes the
tree compile. The two new fields are now wired into the struct
definitions but no surface renders them yet; that remains
bd-b69cf3 scope.

## Validation

- `cargo build -p caco-tui --tests`: clean (0 errors).
- `cargo build -p caco-daemon --tests`: clean.
- `cargo test -p caco-tui --lib`: 2818 / 2818 PASS.
- `cargo test-small`: 209 + 109 + 739 + 291 + 18 + 2818 + 56 PASS.
- `cargo clippy --workspace --all-targets -- -D warnings`: clean.

## Notes / follow-ups

- This is a pure unblocker, not the actual feature. msm-5's
  bd-b69cf3 is the slice that wires the values through SSE
  snapshot pipeline + agent-detail render. They should pull
  this main and continue from there with the structs already
  in shape.
- The Python sweep script is at `/tmp/fix_struct_inits.py` if
  the same shape recurs (struct field added without call-site
  sweep). Reusable for any "missing fields" mechanical
  unblock.
- bd-845653 / bd-58ff27 / bd-c24ff7 / bd-0977ba / bd-de0282
  still on main but bead-close blocked until daemon picks up
  bd-845653 (commit-message bead-id harvester) on next
  operator restart.
