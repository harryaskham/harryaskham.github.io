# bd-bf1e86 polish #11: crons empty state gets keystroke hint parity

## Goal

Bring the `caco-tui` Crons view's empty state to keystroke-hint parity with the Beads view (`Press \`b\` or \`N\` to create a bead, \`r\` to refresh.`) and Notifications view, both of which were polished earlier in this bead's track.

## Bead(s)

- bd-bf1e86 (permanent polish track) — cycle #11

## Before state

`crates/caco-tui/src/views/crons.rs::render_empty` rendered:
```
            No crons configured

  Add daemon crons in your cacophony.yaml under the `crons` key.
```

No keystroke hint, no obvious refresh affordance. Operator landing on an empty crons panel had to either know to press `r`, or leave the TUI to confirm config was correct.

## After state

`render_empty` now appends a hint line styled with `nord::NORD3` (subdued):
```
            No crons configured

  Add daemon crons in your cacophony.yaml under the `crons` key.

           Press `r` to refresh, `?` for help.
```

The existing `render_empty_shows_no_crons` test extended to also assert the keystroke hint is present.

Verification:
- `cargo test -p caco-tui --lib views::crons`: 6/6 PASS
- `cargo test-small`: 57/57 PASS
- `cargo clippy --workspace --all-targets -- -D warnings`: clean

## Diff summary

1 file changed, +14 / −0:

- `crates/caco-tui/src/views/crons.rs`:
  - +9 lines: Line::from("") + 1 styled "Press `r` to refresh, `?` for help." line + bd-bf1e86 polish #11 comment
  - +5 lines: extended unit test assertion for the new hint

## Operator-takeaway

This is the lowest-friction polish in the track: zero behavioural change, pure UX polish, single test extension. Same pattern as polish #1 (beads empty), polish #6 (notifications empty), polish #8 (beads keystroke hint). Empty-state-keystroke-hint parity is now consistent across beads, notifications, and crons. Next candidate views to audit: inbox archived, choices, agents.

bd-bf1e86 cycle counter: 11/session.
