# Session summary — PicoAgentView header chips (bd-42e726)

## Goal

Bring the Android pico header toward macOS/iOS parity: replace the single inline
"state · model · context%" line with distinct Material 3 chips (state, model,
context with color-coding, thinking-effort, queue-depth, activity). Second slice
of the pico-push UX parity work (coordinated with md2-0; macOS reference =
model/context-gauge/effort/queue/activity).

## Bead(s)

- `bd-42e726` — Android PicoAgentView: header model/context/thinking-effort as
  Material 3 chips (pico push; sibling bd-4dd127 thinking styling next)

## Before state

- Failing tests: none.
- PicoHeader rendered one inline labelMedium line "state · model · context%" and
  did not surface thinkingEffort, queueDepth, or activity at all.

## After state

- Failing tests: none. New `PicoHeaderChipsTest` 5/5 green; `compileDebugKotlin`
  + `:app:testDebugUnitTest` SUCCESSFUL.
- PicoHeader now renders a horizontally-scrollable Row of chips: state pill
  (colored by state), model chip, context% chip COLOR-CODED via pure
  `picoContextTone` (Calm <70, Warn 70–90, Critical ≥90), effort chip, queue chip
  (when depth>0), activity chip. Pure label/tone helpers are unit-tested; chip
  colors come from the theme in @Composable helpers.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `ui/pico/PicoAgentView.kt` — chip Row in PicoHeader + pure helpers
    (`picoContextTone`/`picoContextChipLabel`/`picoEffortChipLabel`/
    `picoQueueChipLabel`/`picoActivityChipLabel`) + `PicoChip`/color helpers.
  - test `PicoHeaderChipsTest.kt` (new) — 5 helper tests (tone thresholds,
    labels, blank/zero handling).
- Tests: +5, -0, flipped 0.
- Behavioural delta: pico header now shows distinct, color-coded chips instead of
  one inline line, surfacing effort/queue/activity that were previously hidden.

## Embedded artefacts

- None. Pure tone/label helpers unit-tested; chip visuals need the emulator
  (deferred — ms-dev-2 build-storm; md2-0 will capture post-daemon-update).

## Operator-takeaway

The context chip is color-coded off a single tested threshold function, and the
header now surfaces the same signal set macOS shows (model/context/effort/queue/
activity) from fields PicoAgentViewSnapshot already carried but didn't render.
Second of three coordinated PicoAgentView parity slices; bd-4dd127 (thinking
bubble styling) next.
