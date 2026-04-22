# bd-bf1e86 polish #16: global_beads empty state gets keystroke hint parity

## Goal

Bring the global "Beads — All Projects" view's empty state to keystroke-hint parity with the per-project beads view (polish #8 from earlier this session).

## Bead(s)

- bd-bf1e86 (permanent polish track) — cycle #16

## Before state

`crates/caco-tui/src/views/global_beads.rs::render_empty` (the "no beads anywhere" branch, distinct from the stale/syncing branch which is intentionally instructional in a different way) showed:
```
                  No beads

  No open or recent beads across any project.
```

The per-project beads view (polish #8 / cycle #8 of this session) had `Press \`b\` or \`N\` to create a bead, \`r\` to refresh.` — global beads was missing that parity.

## After state

Appended one styled hint line, same text and style as the per-project beads empty state, only on the non-stale branch. The stale/syncing branch is unchanged because it has its own "waiting for beads data" guidance that better fits that state.

Verification:
- `cargo build -p caco-tui`: clean
- `cargo test-small`: 57/57 PASS
- `cargo clippy --workspace --all-targets -- -D warnings`: clean

## Diff summary

1 file changed, +6 / −0:

- `crates/caco-tui/src/views/global_beads.rs::render_empty`: 6 lines (blank line + styled hint Span + bd-bf1e86 polish #16 comment) on the non-stale branch only

## Operator-takeaway

Last per-project-vs-global parity gap I could find in the empty-state audit. The global beads view is a high-traffic landing surface (hit by `B` global hotkey from anywhere), and the missing keystroke hint left operators without an obvious next-action when projects were brand-new or all beads closed.

This brings the bd-bf1e86 polish track to 16 cycles this session. Next polish-hunting candidates: tab_bar status indicators, console feedback affordances, performance view scroll hints.
