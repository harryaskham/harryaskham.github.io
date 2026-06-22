# Session summary — Send-to-watch tap-ack confirmation

## Why no bead

Same bd-endpoint outage as previous tick. Operator broadcast
("assume you are unblocked and continue progressing things") still
in force. Retroactive bead when bd recovers.

## Goal

Operator polish on the bd-6fc691 Send-to-watch button. The
publish is fast and silent, so the only feedback the operator gets
today is the "Last pushed: HH:mm:ss" row clock ticking over —
easy to miss right at tap time. Add a 2s ephemeral "Sent ✓" green
confirmation that takes over the row whenever a tap-driven push
has just succeeded.

## After state

- `SettingsScreen.WatchAppSection`:
  - New local `var tapAckAtMs by remember { mutableStateOf<Long?>
    (null) }`. Stamped on every Send-to-watch button tap.
  - New derived `ackVisible` flag: true iff there is a tap
    timestamp, the wearRelay `lastDaemonProfilePushAtMs`
    StateFlow has caught up (non-null and ≥ the tap), AND the
    tap is fewer than 2000 ms ago.
  - Last-pushed row text uses a `when {}` block that prefers
    "Sent ✓" while `ackVisible`, otherwise the previous
    sentinel / formatted-timestamp branches. Row color flips
    to `AuroraGreen` while the ack is visible.
- `WatchAppSendToWatchAckSourceTest` (3 tests) pins the tap
  recording, the freshness gate, and the row override + color
  swap.

This is the first slice landing under the new caco-android
profile gradle-assembleRelease before_reintegration gate (landed
1301b73d64 last tick) — `gradle :app:assembleRelease` was run
locally and BUILD SUCCESSFUL before commit, so the gate's queue
or inline run during reintegration should be a quick cache hit.

## Operator-takeaway

Tap Send to watch in phone Settings → Watch App. The row beneath
the button flashes green "Sent ✓" for about 2 seconds, then
reverts to the normal "Last pushed: HH:mm:ss" with the new
timestamp. Auto-publish on connection-config change keeps writing
the timestamp without flashing the ack (only explicit taps do).
