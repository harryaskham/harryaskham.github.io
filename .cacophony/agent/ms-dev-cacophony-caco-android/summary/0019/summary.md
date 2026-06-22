# Session summary — bd-2e66ba chat sidebar hamburger button

## Goal

Make the bd-8233a4 phone-portrait ModalNavigationDrawer discoverable
by adding an explicit Material 3 hamburger IconButton to the
ProjectSelectorBar chrome. Today the only way to open the drawer is
the swipe-from-left-edge gesture, which first-launch operators may
not know exists. This follows Material 3 best practice that drawer
navigation should ALWAYS expose an explicit chrome affordance.

## Bead(s)

- `bd-2e66ba` — Chat sidebar — hamburger button for phone-portrait
  drawer (discoverable affordance).

## After state

- New `LocalChatSidebarOpenDrawer:
  ProvidableCompositionLocal<(() -> Unit)?>` in
  `companion/android/app/src/main/java/com/cacophony/companion/ui/chat/ChatScreen.kt`
  (`staticCompositionLocalOf { null }` default). Mirrors the bd-8233a4
  `LocalChatSidebarSelectionSink` pattern.
- `ChatResponsiveLayout` phone branch wraps its Box(content()) in a
  `CompositionLocalProvider` that installs the local with
  `{ coroutineScope.launch { drawerState.open() } }`. Tablet /
  foldable / landscape branch leaves the local at its null default
  because the fixed rail is always visible.
- `ProjectSelectorBar` reads `LocalChatSidebarOpenDrawer.current` and
  conditionally renders an `IconButton(onClick = openDrawer)` with
  `Icons.Default.Menu` + `contentDescription = "Open chat sidebar"`
  on the leading edge of the chat header Row when the callback is
  non-null (i.e. only on phone-portrait).
- New `ChatSidebarHamburgerSourceTest` (3 tests) pins the
  CompositionLocal default, the phone-branch installation with
  `drawerState.open()`, and the ProjectSelectorBar's conditional
  hamburger render + a11y content description.

## Diff summary

- Files touched (2):
  - `companion/android/app/src/main/java/com/cacophony/companion/ui/chat/ChatScreen.kt`
    (CompositionLocal + phone-branch install + ProjectSelectorBar
    hamburger).
  - `companion/android/app/src/test/java/com/cacophony/companion/ChatSidebarHamburgerSourceTest.kt`
    (new, 3 tests).
- Tests: +3 source-pin tests; no existing tests changed.

## Operator-takeaway

Phones now show a hamburger icon at the leading edge of the chat
header. Tap it to open the Global / Projects / Agents sidebar — same
end state as the swipe-from-left-edge gesture, but operators don't
have to discover the gesture first. Tablet / foldable / landscape
layouts unchanged (no hamburger, rail always visible).
