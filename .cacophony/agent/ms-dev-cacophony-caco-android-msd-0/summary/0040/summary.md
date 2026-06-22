# Session summary — bd-caabe3 slice 2a: group-chat member resolution

## Goal

Add the pure resolution primitive both the Groups display and group-scoped compose
need: split a configured group's members into resolved (known agent) vs unresolved,
and compute self-excluded send targets — per the SPEC group-chat contract.

## Bead(s)

- `bd-caabe3` — Support caco group chat in the Android app chat surface (claimed,
  in_progress). Slice 2a (resolution helper); slice 1 (read model) landed
  @29bbebbd78.

## After state

- `state/GroupChat.kt`: `GroupMemberResolution(groupId, resolved, unresolved)` +
  `resolveGroupMembers(groupId, members, knownAgentIds)` (config-ordered, deduped,
  trimmed, blank-skipping split) + `groupSendTargets(resolution, sender)` (resolved
  members minus the sender, per the loop-prevention rule). Pure — no UI, no
  networking.
- `test/.../GroupChatTest.kt`: 4 tests (resolved/unresolved split, dedupe/trim/blank,
  all-unresolved, self-exclusion).

## Validation

- Queued (host-safe) `gradle :app:testDebugUnitTest --tests "*GroupChatTest"` (job
  tj-0eeca4de) → passed. Pure non-Compose slice → targeted scope. No forbidden
  literals.

## Diff summary

- Code commit: bd-caabe3 slice 2a; landed squash SHA from receipt.
- Files: `state/GroupChat.kt`, `test/.../GroupChatTest.kt`.

## Operator-takeaway

The app can now resolve which configured group members are live vs unresolved and
compute the correct per-member send targets — the foundation for showing groups
(with unresolved members visible) and for group-scoped composition. No user-visible
change yet.

## Remaining gaps

- bd-caabe3 slice 2 (Groups display: ChatSidebarRow.GroupRow + builder + Compose
  rendering + source-pin test updates — a Compose slice needing the full suite),
  slice 3 (group-scoped compose → per-member sends), slice 4 (`via <group>` history
  marker).
