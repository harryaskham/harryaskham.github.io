# Session summary — phone Settings 'Reset watch sync state' two-tap button

## Why no bead

Bd endpoint still down. Operator unblock authorization in force.

## Goal

Operator recovery affordance. When the phone↔watch DataLayer
sync gets stuck on a stale daemon token/host after a config
rotation (e.g. operator rotated the bearer token on the daemon
and the watch is still showing the old "Last imported" snapshot),
there was no in-app way to wipe the published profile without
side-loading adb. `WearRelay.clearDaemonProfile()` (in-tree
since bd-427a40 slice 274) deletes the published DataItem and the
watch's `WatchPhoneDaemonProfile` listener clears its in-memory
snapshot, forcing the operator to re-import on next push — but
that function had no UI entry point.

Add an OutlinedButton at the bottom of the Watch App section
that requires a two-tap arm-then-confirm before calling
`clearDaemonProfile()`.

## After state

- `SettingsScreen.WatchAppSection` (below the Send-to-watch row
  and the Last-pushed / ack line):
  - New two-tap state: `resetArmed`, `resetArmedAt`,
    `resetArmWindowMs = 4000L`. First tap arms (label morphs to
    "Tap again to reset", `AuroraRed` color); second tap inside
    the 4s window commits `wearRelay?.clearDaemonProfile()`.
  - Idle label "Reset watch sync state" with `AuroraOrange` color
    matches the existing destructive-but-recoverable visual
    language used by WatchSettings' Clear-saved-daemon pill.
  - Enabled gate on `isAvailable` so taps cannot leak into the
    `wearRelay` nullable path on non-GMS devices.
- New `WatchAppResetButtonSourceTest` (3 tests) pins the two-tap
  arm-then-confirm with 4s window, the label morph + color flip,
  and the `enabled = isAvailable` gate.
- `gradle :app:assembleRelease` verified BUILD SUCCESSFUL before
  commit.

## Operator-takeaway

Phone Settings → Watch App now has a "Reset watch sync state"
button below "Send to watch" and the last-pushed row. Tap once
to arm (label flashes red "Tap again to reset"); tap again
within 4 seconds to wipe the published DataLayer DaemonProfile.
The watch's WatchSettings "Import from phone" chip will go back
to its initial state until the next phone-side publish. Useful
when token rotation has left the watch stuck on stale state.
