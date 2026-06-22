# Session summary — bd-0eeea6: document the stale-emulator wedge failure mode

## Goal
bd-0eeea6 acceptance item 4: document the stale-emulator node-wedge as a known failure
mode. The reaper mechanism (slices 1-4) and the live cadence-sweep auto-remediation
(slice 5) already landed; this adds the documentation.

## What landed
- `companion/android/QA.md`: new section documenting the symptom (silent
  unix_stream_read_generic hang of all node `.#android` gradle builds), the root cause
  (an abandoned emulator orphaned to PPID 1 holding the `.#android` nix-daemon socket;
  emulator-QA and gradle builds mutually exclusive per node), the operational `ps`
  sequencing check, and the bd-0eeea6 reaper auto-remediation (30-min sweep, 6h+ orphans
  only, never an active QA emulator).

## bd-0eeea6 remaining
Acceptance items still open: (2) agent stop/discard tears down the emulator the agent
launched; (3) a `caco doctor`/`ops` diagnostic surface flags an abandoned emulator with a
remediation hint. Both are integration slices for fresh focus.

## Validation
Markdown-only change. Reint gate is echo-disabled during the merge-train rollout.

## Diff
See the reintegration receipt for the final landed squash SHA.
