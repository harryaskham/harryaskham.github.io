# bd-40d92f + bd-504746 — Pico panes scroll-to-bottom UX

## Goal
Two coordinated pico-pane scroll-behavior UI improvements (md2-0's pico lane,
coordinate-first OK'd; disjoint from their FFI/streaming + bd-bdbec9 boot work):
- bd-40d92f: default to the bottom (latest content) on first load.
- bd-504746: a jump-to-bottom button when scrolled up.

## Bead(s)
- bd-40d92f (Implement default-to-bottom-on-load for pico panes) → CLOSING.
- bd-504746 (Add jump to bottom button for pico panes) → CLOSING.

## Before / After
- Before: the pico transcript LazyColumn started at the TOP on load (the existing
  atBottom auto-follow [bd-1fc12e] only follows when already at the bottom), and
  there was no quick way to jump back to the latest after scrolling up.
- After: (bd-40d92f) a one-time initial scrollToItem(last) once the transcript is
  populated defaults the pane to the latest content; the atBottom auto-follow then
  takes over. (bd-504746) a jump-to-bottom FilledIconButton (the LazyColumn is now
  wrapped in a Box) appears when !atBottom and animates back to the latest item.

## Diff
- companion/android/.../ui/pico/PicoAgentView.kt: PicoTranscript gains
  didInitialScroll + an initial-scroll LaunchedEffect (bd-40d92f); the LazyColumn
  is wrapped in a Box with a jump-to-bottom FilledIconButton
  (Icons.Default.KeyboardArrowDown) shown when !atBottom (bd-504746). Reuses the
  existing listState / atBottom / animateScrollToItem logic.
- companion/android/.../test/.../PicoScrollToBottomSourceTest.kt: NEW source-pin
  test for both.

## Embedded artefacts
- Validated: queued tj-1842320c (gradle :app:testDebugUnitTest --tests
  PicoScrollToBottomSourceTest + PicoAgentViewSourceTest + PicoTranscriptKeyTest +
  PicoTranscriptBubbleStyleTest) — all passed.

## Operator-takeaway
Pico panes now show the latest content by default on open and offer a
jump-to-bottom button when scrolled up. Both beads closing. Coordinated with
md2-0 (their pico lane).

## Diff summary
Landed commit: see the reintegration receipt.
