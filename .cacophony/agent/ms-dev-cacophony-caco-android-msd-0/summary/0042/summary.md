# Session summary — bd-caabe3 slice 3a: group composer target label

## Goal

Add the pure composer-header label for a selected group chat, so the group-scoped
compose slice (3b) can show which group + how many members a send will reach.

## Bead(s)

- `bd-caabe3` — caco group chat in the Android chat surface (claimed, in_progress).
  Slice 3a (composer label); slices 1, 2a, 2 landed (read model, resolution,
  display @58f866892d).

## After state

- `state/GroupChat.kt`: `groupComposeTargetLabel(resolution)` → e.g. "Group
  android-team (2 members)" or "Group android-team (2 members, 1 unresolved)".
  Shows the resolved member count the send will reach plus any unresolved members
  (SPEC: unresolved stay visible). Pure.
- `test/.../GroupChatTest.kt`: +1 test (singular/plural members, unresolved suffix,
  empty group).

## Validation

- Queued (host-safe) `gradle :app:testDebugUnitTest --tests "*GroupChatTest"` (job
  tj-0e3c4bac) → passed. Pure non-Compose → targeted scope. No forbidden literals.

## Diff summary

- Code commit: bd-caabe3 slice 3a; landed squash SHA from receipt.
- Files: `state/GroupChat.kt`, `test/.../GroupChatTest.kt`.

## Operator-takeaway

The label a group composer will show is now computed + tested. No user-visible change
yet — slice 3b wires the actual group-scoped send (per-member expansion) and surfaces
this label in the composer.

## Remaining gaps

- bd-caabe3 slice 3b (group-scoped send: expand to resolved members + per-member
  sendMessage + per-target diagnostics, show the label in the composer — a Compose +
  send slice needing the full suite), slice 4 (`via <group>` history marker).
