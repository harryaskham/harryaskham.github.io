# Session summary — WearOS TTS blank-safe set errors

## Goal

Polish WearOS TTS voice/profile set-failure copy so whitespace-only backend error strings render useful fallback text.

## Bead(s)

- `bd-9be92f` — WearOS TTS set errors avoid blank copy

## Before state

- Failing tests: none in the focused validation lane.
- Relevant metrics: TTS voice/profile set failures rendered `Set ${name} failed: ${r.message}` directly, so blank/whitespace messages could produce blank-looking failure copy.
- Context: focused WearOS TTS UI copy polish; no TTS set request behavior changes.

## After state

- Failing tests: none.
- Relevant metrics: added pure `watchTtsSetErrorCopy(kind, name, message)` helper; voice/profile set errors trim item names/details, falling back to `voice` / `profile` and `unknown error` when blank.
- Context: no-daemon and successful set copy unchanged.

## Diff summary

- Code/content commits: `bd-9be92f: make WearOS TTS set errors blank-safe`; final landed squash SHA comes from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched: `WatchTtsVoicesScreen.kt`, `WatchTtsProfilesScreen.kt`, `WatchTtsSetVoiceSourceTest.kt`, `WatchTtsSetProfileSourceTest.kt`.
- Tests: `tj-1c89c61f` passed `WatchTtsSetVoiceSourceTest` + `WatchTtsSetProfileSourceTest`; `bj-510ea92e` succeeded (`:wearable:assembleRelease`).

## Operator-takeaway

WearOS TTS voice/profile set failures now show `unknown error` instead of blank failure details.
