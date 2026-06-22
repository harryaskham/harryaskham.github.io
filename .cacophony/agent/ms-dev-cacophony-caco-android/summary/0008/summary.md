# Session summary — bd-967f07 revert wearable standalone=true (P1 connectivity regression)

## Goal

Revert the bd-b9726c flip of `com.google.android.wearable.standalone`
from `false` to `true`. Operator (Harry) reported on 2026-06-01 that
the watch on the latest build can't connect to the phone — no node
token copy to watch, no BLE, phone "not found" / "idle". Root cause
was the standalone-true flag breaking Wear OS's companion-app pairing
heuristic.

## Bead(s)

- `bd-967f07` — Revert wearable standalone=true: watch can't pair
  phone / no DataLayer / no BLE (P1 bug).
- Regression source: `bd-b9726c` — Wear OS AAB bundled into Play
  release pipeline (the bundling work was correct; the manifest flip
  was a wrong implication drawn from "watch UI renders standalone").

## Before state

- `companion/android/wearable/src/main/AndroidManifest.xml` declared
  `com.google.android.wearable.standalone = true` (bd-b9726c).
- Watch lost the phone→watch DataLayer push of the daemon profile
  (slice 274 bd-427a40 `publishDaemonProfile`), so the "Import from
  phone" chip never fires.
- BLE pairing channel goes idle because Wear OS doesn't actively
  maintain it for standalone-declared apps.
- Operator hits "phone not found / idle" on the watch and has no path
  to load the node token short of typing it on the wrist.

## After state

- Manifest reverts to `standalone=false`.
- Comment expanded to spell out the bd-b9726c root cause, the
  operator-reported symptom, the difference between "standalone UI
  shell renders" (true) and "standalone connectivity" (false), and an
  explicit do-not-re-flip-without-evidence guard for future agents.
- New `WearStandaloneManifestSourceTest` (2 tests) pins:
  the standalone=false declaration, the absence of any
  standalone=true occurrence in the file, the comment's references
  to bd-77d1f4 + bd-967f07 + the publishDaemonProfile / BLE /
  DataLayer symptoms, and the explicit "Do NOT re-flip to true
  without operator-side reproduction" guard text.

## Diff summary

- Code commit: pending final squash SHA from reintegration receipt.
- Files touched (2):
  - `companion/android/wearable/src/main/AndroidManifest.xml`
    (one meta-data value flip + comment expansion).
  - `companion/android/wearable/src/test/java/com/cacophony/companion/WearStandaloneManifestSourceTest.kt`
    (new, 2 tests).
- Tests: +2 source-pin tests; no existing tests changed.
- Behavioural delta: once the watch+phone APKs are rebuilt from this
  commit and installed, Wear OS's companion pairing detection fires
  again. BLE channel stays warm, WearRelay's DataLayer push reaches
  the watch, `publishDaemonProfile` copies the node token to the
  watch on every phone-side configure(), and the "Import from phone"
  chip on the watch consumes that profile in one tap.

## Embedded artefacts

- None this session.

## Operator-takeaway

The bd-b9726c bundling work is unaffected — the Play release pipeline
keeps shipping phone and wear AABs in one workflow run with the same
upload key. Only the wrong manifest implication is reverted. The
expanded comment + source-pin tests make it harder for a future agent
to re-flip the value without first reproducing connectivity
end-to-end, which was the gap that let bd-b9726c land the regression.
