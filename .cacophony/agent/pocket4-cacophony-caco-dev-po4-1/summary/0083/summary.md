# Session summary — on-droid chat border repair

## Goal

Fix Harry's operator-reported on-droid TUI chat bubble border regression where the shared separator between adjacent bubbles showed literal ellipses/truncated box drawing and placed the next bubble title on the wrong side of the previous bubble footer.

## Bead(s)

- `bd-1dc073` — Fix on-droid TUI border truncation and broken borders

## Before state

- Failing tests: none known for this specific regression at session start.
- Relevant metrics: Harry supplied visual/text evidence with `evt-019d85a7`, `evt-019d85a8`, and `evt-019e7d93` showing `╰ ▶ ─────...` style shared rows on nix-on-droid.
- Context: the current chat bubble renderer had moved message metadata to footer rows and merged adjacent bubble separators, but the merge path truncated the previous footer with `common::truncate` and rendered the next title on the right-hand segment.

## After state

- Failing tests: targeted chat tests pass; full `caco-tui --lib` run had one unrelated app remote-tmux test fail once and pass when rerun alone.
- Relevant metrics: `cargo test -p caco-tui views::chat::tests:: -- --nocapture` passed 89/89 chat tests; `cargo test -p caco-tui app::tests::visible_remote_tmux_error_does_not_backoff_foreground_pane_bd_ace26c -- --nocapture` passed 1/1 on rerun.
- Context: shared chat separators now start from the previous bubble's real footer, overlay the next bubble title on the left side, keep play/node/project/timestamp footer controls right-aligned, and avoid ellipsis in border rows.

## Diff summary

- Code/content commits: `db83e36a8f`.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched: `crates/caco-tui/src/views/chat.rs`.
- Tests: +1 regression test for the on-droid shared separator layout; existing chat tests updated for the new left-title/right-footer contract.
- Behavioural delta: adjacent chat bubble shared separator rows now match the intended composition: next bubble agent title on the left, previous bubble play/node/project/time footer on the right, without literal `...` truncation corrupting border glyphs.

## Operator-takeaway

The regression was in the chat bubble shared-separator composition, not Android-specific rendering: the code was truncating a border string and placing the next title in the right segment. The fix composes the shared row from cells so narrow/on-droid terminals do not get ellipsis-corrupted borders.
