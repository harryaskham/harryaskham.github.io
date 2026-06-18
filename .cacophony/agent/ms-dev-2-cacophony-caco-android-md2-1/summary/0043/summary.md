# Session summary — bd-f7fa73 (Android App Shortcuts, reverse iOS parity)

## Goal
Add Android launcher App Shortcuts (long-press the app icon) for reverse parity with iOS App/AppShortcuts.swift — the App Shortcuts half of bd-f7fa73 (the QR-scan connect half is split as a follow-up).

## Bead(s)
- bd-f7fa73 (claimed; App Shortcuts part landed; QR-scan connect remains as a noted follow-up).

## Before state
Android had no launcher App Shortcuts (grep for shortcuts.xml / android.app.shortcuts was empty), while iOS exposes AppShortcuts (AppIntents). Phone users had no long-press quick actions.

## After state
Three static launcher shortcuts — Quick file, Choices, Chat — via res/xml/shortcuts.xml + a manifest android.app.shortcuts meta-data on MainActivity. Each shortcut launches MainActivity with a navigate_to extra reusing the EXISTING deep-link targets (quick-bead -> showQuickFile, choices -> inbox/choices, chat -> Chat). No change to the navigate_to dispatch logic, so bd-618fc6's NotificationNavExtraSourceTest pins stay green (verified).

## Diff summary
Landed squash-merged on main — see the reintegration receipt for the final landed SHA. New files: res/xml/shortcuts.xml, res/drawable/ic_shortcut_{chat,choices,quick_file}.xml (3 vector icons), AppShortcutsSourceTest.kt (3 source pins). Edits: res/values/strings.xml (+6 shortcut label strings), AndroidManifest.xml (+ additive meta-data child on the MainActivity activity).

## Embedded artefacts
- Full :app:testDebugUnitTest: 1822 prior tests 0 failures (no regression; NotificationNavExtraSourceTest green) + new AppShortcutsSourceTest 3/0.
- :app:assembleDebug: success (resources + manifest merge validated).
- Emulator render skipped (low-risk static resource + manifest change; the deep-link targets are already exercised by existing navigate_to tests).

## Operator-takeaway
Found + claimed a genuine in-lane parity bead during steady-state, coordinated the additive manifest touch with md2-0 (no conflict; kept the navigate_to dispatch unchanged per their bd-618fc6 pin guidance), and landed the App Shortcuts half. QR-scan connect (CameraX/MLKit) is the remaining bd-f7fa73 follow-up.
