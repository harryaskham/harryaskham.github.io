# Session summary — bd-0eeea6 item 3: caco doctor abandoned-emulator diagnostic

## Goal
bd-0eeea6 acceptance item 3: a diagnostic surface (caco doctor) flags an abandoned
.#android emulator holding the build path before it wedges the node, with the
remediation hint. The reaper mechanism + live cadence sweep (item 1) and the docs
(item 4) already landed.

## What landed
- `crates/caco-daemon/src/doctor_deep.rs`: `DeepDoctorStaleStateKind::AbandonedAndroidEmulator`
  (+ label) and `sample_deep_doctor_abandoned_android_emulators(candidates, min_age, max)`
  — pure, filters enumerated candidates to reapable 6h+ orphans (ppid==1, no live owner)
  and builds observations with evidence + a kill remediation. Unit-tested.
- `crates/caco-cli/src/lib.rs`: `doctor_abandoned_emulator_check_from_findings` (renders a
  `DoctorCheck`, ok when none / warning when abandoned emulators found) and
  `doctor_abandoned_emulator_scan_local_checks` (local-only /proc enumeration via
  `enumerate_android_emulator_candidates` -> the sampler -> a check + remediation hint),
  wired into the main `caco doctor` aggregation as section 4e alongside the zombie scan.

## bd-0eeea6 remaining
Item 2: agent stop/discard tears down the emulator the agent launched. Then the bead is
feature-complete and closes.

## Validation
`cargo check -p caco-cli` (tj-1183d9e7) PASSED (compiles caco-cli + caco-daemon). The
doctor_deep sampler unit test passed earlier (tj-7db2001b). Reint gate echo-disabled;
self-validated before landing.

## Diff
See the reintegration receipt for the final landed squash SHA.
