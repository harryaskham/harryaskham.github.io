# Session summary — Fix Android card hover square-border artifact (bd-88fa60)

## Goal

Remove the "weird square inner border" that appears when hovering a card with a
mouse in the Android app. Root-caused to an unclipped bounded ripple on the
workhorse `AccentCard`, and fixed by moving the click target onto the card's
clipped content so the hover overlay follows the rounded corners.

## Bead(s)

- `bd-88fa60` — Fix weird square inner border on card hover in Android app (P2 bug)

## Before state

- Failing tests: none.
- Root cause (`ui/components/Components.kt`, `AccentCard`): the onClick+onLongClick
  path applied `combinedClickable(indication = ripple(bounded = true))` to the
  Card's OUTER modifier (`modifier.fillMaxWidth().combinedClickable(...).then(
  accentDraw)`). The bounded ripple's mouse-hover overlay is drawn in the card's
  RECTANGULAR bounds; the Card content is rounded, so on hover the overlay bleeds
  past the rounded corners and reads as a square inner border. AccentCard backs
  most list cards (Beads/Agents/Jobs/etc.), so the artifact showed app-wide. The
  other paths were already fine: `Card(onClick=...)` uses Material3's own
  shape-clipped ripple, and the long-press-only path uses `indication = null`.

## After state

- Failing tests: none. New `AccentCardHoverBorderSourceTest` 1/1 green; existing
  `ComponentsSourceTest` / `ComponentsModifierParameterSourceTest` /
  `ComponentsPerRowBrushRememberSourceTest` still green; `compileDebugKotlin` +
  `compileDebugUnitTestKotlin` clean.
- The onClick+onLongClick path now makes the Card a non-clickable elevated
  container and moves the `combinedClickable(... ripple(bounded = true) ...)` onto
  the inner content `Column`. Material3's Surface clips card content to the
  rounded shape, so the hover ripple is clipped and no longer overflows the
  corners. `accentDraw` (accent wash) and the long-press flash stay on the OUTER
  modifier, so the accent compositing (behind the semi-transparent surface) and
  the drop shadow/elevation are unchanged.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `ui/components/Components.kt` — AccentCard onClick+onLongClick path: clickable
    + bounded ripple moved from the Card outer modifier to the content Column;
    removed the now-dead `rowModifier` branch.
  - test `AccentCardHoverBorderSourceTest.kt` (new) — pins that a content
    `Column(` opens before the bounded ripple (ripple is clipped to content).
- Tests: +1, -0, flipped 0.
- Behavioural delta: mouse-hover ripple/overlay on AccentCard is clipped to the
  rounded shape; no square corner artifact.

## Embedded artefacts

- None. This is a mouse-hover visual artifact; reproducing it needs a pointer
  device over the running app and no emulator AVD is provisioned on this node, so
  validation is the structural fix (a well-known Compose unclipped-bounded-ripple
  gotcha) plus a source regression pin and clean compile. A pointer-hover visual
  capture would be a good follow-up confirmation once an AVD + mouse are available.

## Operator-takeaway

The square hover border was a classic Compose pitfall: a `bounded = true` ripple
on a clickable that sits OUTSIDE the rounded-shape clip draws in the element's
rectangular bounds. Putting the clickable on the Card's clipped content is the
standard remedy, and keeping `accentDraw`/flash/shadow on the outer modifier
means the only behavioural change is that the hover overlay now respects the
rounded corners — no accent or elevation regression. Because AccentCard is the
shared list-card, this one change covers the artifact across the app.
