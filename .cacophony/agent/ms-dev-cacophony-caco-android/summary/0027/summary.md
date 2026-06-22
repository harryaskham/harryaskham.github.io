# Session summary — bd-6fc691 Phone Settings 'Send to watch' button + last-pushed timestamp

## Operator report (2026-06-02)

> "I don't see a way to sync the phone and watch or send token…
> maybe I am on old version? in any case check"

Investigation: phone↔watch daemon profile push exists via
bd-427a40 slice 274 (`WearRelay.publishDaemonProfile`) and fires
automatically on every connection-config change in
`MainActivity.kt:278`. The watch already shows an "Import from
phone" chip in `WatchSettingsScreen.kt:298-383`. But the phone side
had no visible button and no in-app evidence that any publish
fired, so the operator couldn't confirm the sync without
side-loading adb / logcat.

Two sibling operator-report beads were filed for the other issues
the operator raised: bd-83c33d (wear complications render blank)
and bd-ae30e2 (terminal shows nix-on-droid daemon user, not the
selected node's user — UX advisory + cross-node PTY proxy
follow-up).

## Bead(s)

- `bd-6fc691` — Phone Settings — explicit 'Send to watch' button +
  last-pushed timestamp.

## After state

- `WearRelay` exposes new
  `lastDaemonProfilePushAtMs: StateFlow<Long?>` (null sentinel =
  never published in this process lifetime). The backing
  `MutableStateFlow` is updated inside `publishDaemonProfile` on
  every successful putDataItem dispatch so both the auto-publish on
  config save AND the new explicit Send-to-watch button update the
  same timestamp.
- `SettingsScreen.WatchAppSection` now accepts a nullable
  `connectionManager: ConnectionManager? = null` parameter and the
  caller passes the active manager.
- The section renders, below the pairing-status card and install
  line:
  - An OutlinedButton labeled "Send to watch" with
    `Icons.AutoMirrored.Filled.Send`, enabled only when the relay
    is available AND there is a saved daemon config to publish.
    Falls back to "No daemon config to send" copy when `cfg` is
    null. Tap calls `wearRelay.publishDaemonProfile(cfg.host,
    cfg.port, cfg.token)`.
  - A "Last pushed: <HH:mm:ss>" / "Last pushed: never" row that
    subscribes to the new StateFlow with `collectAsState(initial =
    null)`.
- New internal helper `formatWallClockHms(epochMs: Long): String`
  produces the deterministic `HH:mm:ss` string used by the row;
  pulled out so the source-pin test exercises it directly.
- New `WatchAppSendToWatchSourceTest` (5 tests): WearRelay
  StateFlow + update site, WatchAppSection param + caller wiring,
  button render + label + onClick wiring, last-pushed row
  subscription + 'never' sentinel + formatted variant, and a
  deterministic UTC/Locale.US format check pinning
  `formatWallClockHms(1780404896000L) == "12:34:56"`.

## Diff summary

- Files touched (3):
  - `companion/android/app/src/main/java/com/cacophony/companion/relay/WearRelay.kt`
    (StateFlow imports + backing flow + update site).
  - `companion/android/app/src/main/java/com/cacophony/companion/ui/settings/SettingsScreen.kt`
    (caller wiring + section param + button + last-pushed row +
    helper).
  - `companion/android/app/src/test/java/com/cacophony/companion/WatchAppSendToWatchSourceTest.kt`
    (new, 5 tests).
- Tests: +5 source-pin + unit tests; no existing tests changed.

## Operator-takeaway

Open phone Settings, scroll to the "Watch App" section. You will now
see a "Send to watch" button below the pairing-status card, plus a
"Last pushed: HH:mm:ss" row underneath. Tap the button any time to
force-resend the daemon host/port/token to the watch; the timestamp
updates after each successful push. If the button is disabled,
either the relay is unavailable (no Wear-paired GMS device) or you
haven't saved a daemon config yet.

On the watch side, the corresponding "Import from phone" /
"Refresh from phone" chip is already in Settings (it has been since
slice 274 / bd-427a40); tap it to import the profile.

Two follow-up operator-report beads are open for the other issues
raised: bd-83c33d (wear complications blank) and bd-ae30e2
(terminal username mismatch).
