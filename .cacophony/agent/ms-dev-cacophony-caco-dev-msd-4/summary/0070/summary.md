# Session summary — Android Chat replaces Inbox navigation

## Goal

Replace the Android companion app's primary Inbox bottom-navigation destination with a dedicated Chat destination while preserving recent navigation contracts: Timeline remains under More, and the More bottom-nav geometry/tap-target behavior stays unchanged.

## Bead(s)

- `bd-37a5b0` — Replace Inbox navigation with Chat

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: bottom navigation exposed Overview, Inbox, Beads, Agents, Feed, More; Chat was reachable only as a More sub-page.
- Context: the companion owner explicitly asked for the full Android unit gate before reintegration and reminded that Android emulator/QEMU work must not run on ms-mac.

## After state

- Failing tests: none in validation.
- Relevant metrics: bottom navigation now exposes Overview, Chat, Beads, Agents, Feed, More; More sub-page coverage no longer expects Chat; notification taps request `navigate_to=chat` instead of `inbox`.
- Context: Chat uses the existing unread operator-inbox/choice badge count so message/choice attention still appears in primary navigation.

## Diff summary

- Commits: source branch commit `d1718a651` after rebase/conflict resolution; reintegration will squash this into a mainline commit.
- Files touched: `companion/android/app/src/main/java/com/cacophony/companion/MainActivity.kt`, `companion/android/app/src/main/java/com/cacophony/companion/notifications/CacophonyNotificationManager.kt`, `companion/android/app/src/test/java/com/cacophony/companion/FullAppNavigationTest.kt`.
- Tests: updated full-app navigation tests to click the Chat bottom tab and remove More > Chat expectations.
- Behavioural delta: primary navigation now opens `ChatScreen` directly from the Chat tab; the old Inbox tab and duplicate More > Chat menu entry are gone.
- Validation: `cargo fmt --all -- --check`; `cd companion/android && nix develop -c gradle :app:testDebugUnitTest --tests com.cacophony.companion.FullAppNavigationTest --no-daemon`; `cd companion/android && nix develop -c gradle :app:testDebugUnitTest --no-daemon`.

## Operator-takeaway

Android now treats Chat as a first-class destination rather than hiding it behind More, while the legacy operator-inbox data layer remains available behind the badge and notifications.
