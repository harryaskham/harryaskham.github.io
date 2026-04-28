# Session summary — TUI chat bubble border cleanup

## Goal

Fix the P0 TUI rendering regression where bitmap borders from chat bubbles could remain visible after navigating away from chat bubble surfaces or switching to another UI section.

## Bead(s)

- `bd-fbe467` — Fix TUI border redraw issues when navigating away from chat bubbles

## Before state

- Failing tests: no dedicated regression coverage for agent-detail Chat-tab inner navigation clearing bitmap bubble placements.
- Relevant metrics: existing content swap cleanup only keyed by top-level `ContentPane` per tile ID.
- Context: top-level chat-to-inbox swaps were partly covered, but agent detail inner-tab swaps occur inside a single `ContentPane`, so leaving Chat for Logs/Home could skip the explicit kitty delete path.

## After state

- Failing tests: none in the targeted validation run.
- Relevant metrics: `cargo test -p caco-tui lifecycle_tag -- --nocapture` passed; `cargo test -p caco-tui kitty_lifecycle_harness -- --nocapture` passed; `cargo check -p caco-tui --tests` passed.
- Context: content lifecycle tracking now includes agent-detail inner-tab identity and clears the tile plus stale graphics caches before re-rendering the replacement view.

## Diff summary

- Commits: `500c86714`
- Files touched: `crates/caco-tui/src/app.rs`
- Tests: added 2 focused unit/regression tests and updated 1 existing kitty lifecycle harness test for the new lifecycle key shape.
- Behavioural delta: content/sub-view swaps now clear the affected tile text cells, invalidate retained kitty border/background placements, clear stale background cache entries, and detect agent chat-tab exits even when the top-level pane remains `AgentDetail`.

## Operator-takeaway

The stale chat-bubble border issue was a lifecycle-key gap rather than a bubble renderer problem: agent chat tabs could change scenes without changing the outer pane identity. The fix makes those sub-view transitions participate in the same explicit graphics cleanup path as full pane navigation.
