# Session summary — WatchSettings app-version footer

## Why no bead

Bd endpoint still down. Operator unblock authorization in force.

## Goal

Wear parity with the phone-side AppVersionFooter slice (landed
last tick at 919b3f1134). Same operator diagnostic problem on the
watch: WatchSettings showed daemon connection info but nothing
about which build of the wearable was actually installed.
Operators picking the right Play release between phone
(`internal`, vc base) and wear (`wear:internal`, vc base +
1_000_000 per bd-b680e3) had no in-app way to confirm.

Add a small grey footer line as the last item in WatchSettings'
ScalingLazyColumn:
`Cacophony Wear <versionName> (vc <longVersionCode>)`.

## After state

- `companion/android/wearable/src/main/java/com/cacophony/companion/wear/settings/WatchSettingsScreen.kt`:
  - New `item { WatchAppVersionFooterItem(context = context) }`
    rendered right after the existing "Back" chip item.
  - New private composable
    `WatchAppVersionFooterItem(context: android.content.Context)`
    reads `context.packageManager.getPackageInfo(pkg, 0)`,
    formats `"Cacophony Wear $name (vc $code)"` via
    `info.longVersionCode` (Int isn't wide enough once you add
    the +1_000_000 wear offset for some hosts), swallows any
    throwable with `"Cacophony Wear"` fallback so a missing
    package info edge case doesn't crash WatchSettings.
  - Footer rendered as a single Text in `SettingsDim` color at
    9sp — matches the surrounding `shutdownMessage` style for
    visual consistency.
- New `WatchSettingsAppVersionFooterSourceTest` (3 tests) pins
  the composable definition, the packageManager call + label
  format + throwable swallow, and the WatchSettings ScalingLazyColumn
  invocation right after the Back chip.
- `gradle :wearable:assembleRelease` verified BUILD SUCCESSFUL.

## Operator-takeaway

Scroll to the very bottom of the watch Settings screen. The new
grey line shows e.g. "Cacophony Wear 1.2.1063-731d52f7 (vc
1012421)" so the operator can confirm at a glance which build
is running, including the +1M wear offset that lines up with the
wear:internal Play track upload.
