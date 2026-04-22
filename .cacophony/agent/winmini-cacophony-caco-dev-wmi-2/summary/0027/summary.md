# bd-bf1e86 polish #14: fuzzy picker differentiates empty-query vs no-match + adds Esc hint

## Goal

Improve the fuzzy picker's empty state from a single "No matches." line to a context-aware message + dismissal hint, fixing the confusion of "is this picker even working?" when the operator opens it on an empty workspace.

## Bead(s)

- bd-bf1e86 (permanent polish track) — cycle #14

## Before state

`crates/caco-tui/src/views/fuzzy_picker.rs::render` showed exactly one DIM line "  No matches." for both empty-query (the picker just opened, nothing to match against) AND non-empty-query-no-results states. No keystroke hint for `Esc` to dismiss.

## After state

Branch on `query.is_empty()`:
- empty: "No items available." + "Press Esc to dismiss."
- non-empty: "No matches." + "Backspace to refine, Esc to dismiss."

Hint line styled in plain nord::NORD3 (not DIM) so it's readable while remaining subdued.

Verification:
- `cargo test -p caco-tui --lib views::fuzzy_picker`: 2/2 PASS
- `cargo test-small`: 57/57 PASS
- `cargo clippy --workspace --all-targets -- -D warnings`: clean

## Diff summary

1 file changed, +18 / −4:

- `crates/caco-tui/src/views/fuzzy_picker.rs::render`: replaced single-line empty branch with branched message + hint pair

## Operator-takeaway

Fuzzy picker is one of the highest-traffic interaction surfaces (entry point for every cross-pane navigation) and was missing the Esc hint that exists in modal overlays elsewhere. The empty-query case is also more useful than "No matches." (which implies user-error when in fact there's just nothing to search yet).

Polish #11-#14 form a cluster of "first-render-gives-orientation" fixes: crons, hooks, profiles empty states all now include `r`/`?` hints; fuzzy picker now includes Esc + Backspace hints. The remaining audit candidate (chat empty state) was already polished by peer in a recent commit.

bd-bf1e86 cycle counter: 14/session.
