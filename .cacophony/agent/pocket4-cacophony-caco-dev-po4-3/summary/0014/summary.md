# Session summary — TUI graphics placement stabilization

## Goal

Respond to Harry's report that TUI graphics still flicker when identical bitmap chrome appears at different screen positions, with related missing navigation subpanel backgrounds and broken text-mode chat bubble rendering.

## Bead(s)

- `bd-2b0b80` — Fix TUI graphics flicker and missing chat/navigation backgrounds

## Before state

- Failing tests: no pre-existing targeted failures known; the symptom was operator-observed live TUI flicker/incorrect rendering.
- Relevant metrics: production `SurfaceManager::new()` allowed cross-surface retained-image sharing through the global content-hash retained-image lookup, meaning distinct panels could display one retained Kitty image ID with multiple placement IDs.
- Context: chat bubble text-mode lines used character counts in several width-sensitive paths, which can overflow or misalign when sender labels and message text include wide Unicode or very long identifiers.

## After state

- Failing tests: none in targeted validation; one unrelated existing warning remains from `caco-daemon` about an unused `LifecycleOperationError` import.
- Relevant metrics: targeted retained-image tests, production no-cross-share test, and the full `views::chat::tests::` filter pass after rebasing over concurrent chat bubble work.
- Context: production TUI now uses surface-local retained image reuse instead of cross-surface image-ID sharing, while legacy shared-retention behavior remains covered only through the explicit `with_capability` test constructor. Chat bubble lines now clamp by display width and keep non-graphics bubble backgrounds on padding/right-border cells.

## Diff summary

- Code/content commits: `0e25bf50ca`
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `crates/caco-tui/src/kitty.rs`, `crates/caco-tui/src/views/chat.rs`
- Tests: added production retained-image no-cross-share coverage and wide-sender chat bubble width coverage; ran retained-image and chat test filters.
- Behavioural delta: byte-identical navigation/background/bubble graphics in different places no longer share one Kitty image ID in production, avoiding image/virtual-placement mixups. Text-mode chat bubbles clamp wide labels and preserve fallback background styling.

## Operator-takeaway

The safest fix was to trade a little retained-image dedupe performance for correctness: production no longer reuses one terminal image ID across distinct logical surfaces, which should stop identical-but-differently-placed chrome from blinking or disappearing as panels move.
