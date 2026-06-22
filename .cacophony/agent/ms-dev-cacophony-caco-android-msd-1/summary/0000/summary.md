# Session summary — caco-android single-builder rule capture (bd-c4b24c)

## Goal

Revived as the caco-android decomposition lead on ms-dev and acted on Harry's
android pico-overhaul directive. The first concrete deliverable was bd-c4b24c:
durably capture the Android/WearOS single-builder release rule in the
caco-android profile so heavy release builds and Play rollouts are owned by
caco-android-releaser while feature workers build fast and reintegrate to HEAD.
Also established explicit android-team ownership and armed two autonomous loops.

## Bead(s)

- `bd-c4b24c` — caco-android: single-builder coordination — caco-android-releaser
  owns heavy release builds; other caco-android workers build fast and
  reintegrate to HEAD (P1, claimed + implemented this session).
- Context-only (not modified here): `bd-257184` (md2-0, Android pico live
  event-delta FFI parity), draft UX beads `bd-42e726`/`bd-838d98`/`bd-4dd127`
  (md2-1, PicoAgentView UX parity). Sibling of macOS `bd-1201ac`.

## Before state

- Failing tests: none touched (docs/profile change).
- Profile `.cacophony/profiles/caco-android.md` had a "Heavy-build ownership"
  paragraph attributed only to the macOS bead `bd-1201ac`, not the Android
  sibling, and did not spell out releaser-owned validation, the host-aware
  folding, or the shared single-owner heavy-build slot.
- A live `aurora-cacophony-caco-android-releaser` agent already exists, so the
  releaser role is real; the rule just was not captured durably as Android's own.

## After state

- Failing tests: none.
- `.cacophony/profiles/caco-android.md` now has a "Single-builder ownership /
  build pacing" section attributed to `bd-c4b24c` (Android sibling of macOS
  `bd-1201ac` + caco-ios), spelling out: feature workers build fast + reintegrate
  to HEAD and do not run signed release builds/Play uploads; caco-android-releaser
  owns the heavy signed `:app`/`:wearable` build + validation + phone/wear Play
  rollout; one heavy build at a time via a single-owner slot coordinated through
  the decomposition lead; folds into the existing host-aware no-local-heavy-build
  rule; routine Play cadence defers to the releaser + `android-release-cadence`.
- Two autonomous loops armed (20m coordination sweep, 45m pico-UX polish sweep).

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `.cacophony/profiles/caco-android.md` (one section rewrite).
- Tests: +0 / -0 / flipped 0 (docs/profile change).
- Behavioural delta: caco-android workers now have a durable, Android-specific
  single-builder rule; no runtime/code behavior changed.

## Operator-takeaway

The Android/WearOS single-builder rule is now captured in the caco-android
profile as its own bead (bd-c4b24c), not just inherited from the macOS bead.
caco-android-releaser owns heavy release builds + Play rollout; feature workers
reintegrate to HEAD. This is coordination/docs only — the live releaser agent
(aurora) already exists, so this makes the existing arrangement explicit and
enforceable rather than introducing new mechanism.
