# bd-bf1e86 polish #13: profiles empty state gets keystroke hint parity

## Goal

Add the standard `Press \`r\` to refresh, \`?\` for help.` keystroke hint to `crates/caco-tui/src/views/profiles.rs::render_empty`, matching the pattern landed in polish #11 (crons) and #12 (hooks).

## Bead(s)

- bd-bf1e86 (permanent polish track) — cycle #13

## Before state

The profiles empty state already had a discovery-paths hint (the search-order list `.cacophony/profiles/ > configs/profiles/ > ~/.cacophony/profiles/ > embedded`), but no keystroke affordance. Operator landing on a 0-profile state had to know `r` was bound.

## After state

One styled hint line appended after the discovery-paths line, same nord::NORD3 subdued style as #11/#12. No test added (the file has no existing render_empty test to extend; adding one would expand scope past the polish discipline).

Verification:
- `cargo build -p caco-tui`: clean
- `cargo test-small`: 57/57 PASS
- `cargo clippy --workspace --all-targets -- -D warnings`: clean

## Diff summary

1 file changed, +5 / −0:

- `crates/caco-tui/src/views/profiles.rs`: 5 lines (blank + styled hint Span + bd-bf1e86 polish #13 comment)

## Operator-takeaway

Empty-state-keystroke-hint parity now consistent across: beads, notifications, crons, hooks, profiles. Three views remain in the audit list from #11: builds, releases, tests. Will keep the same bundled-polish cadence per cycle.

bd-bf1e86 cycle counter: 13/session.
