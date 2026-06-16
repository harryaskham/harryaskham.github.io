# Session summary — stable pico transcript LazyColumn keys (bd-99ccc6)

## Goal

Make the Android pico transcript scroll-stable under md2-0's bd-257184 FFI live
stream: the FFI keep-fresh loop publishes a full reduced PicoAgentViewSnapshot
every tick, so PicoTranscript's keyless LazyColumn would re-key on every
replacement and jump scroll mid-stream. Add stable item keys (md2-0's FFI-design
review surfaced this; keys-only change in my file, zero conflict with their
FfiPicoSessionSource).

## Bead(s)

- `bd-99ccc6` — Android pico: stable LazyColumn item keys in PicoTranscript

## Before state

- Failing tests: none.
- `PicoTranscript` used keyless `items(snapshot.transcript)` + keyless streaming/
  status `item{}`s, so full-snapshot replacement per FFI tick lost row identity
  and would jump scroll during streaming.

## After state

- Failing tests: none. New `PicoTranscriptKeyTest` 4/4 green; compileDebugKotlin
  + `:app:testDebugUnitTest` SUCCESSFUL.
- Committed transcript rows use a stable `picoTranscriptKey(item, index)` =
  `pico-item-<index>-<type>` (append-only, so index+type keeps identity across
  replacement); streaming-thinking/blocks/text + status pins use fixed keys, so
  the streaming row updates in place. Snapshot contract + PicoAgentView signature
  unchanged (md2-0's FFI drop-in unaffected).

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `ui/pico/PicoAgentView.kt` — `itemsIndexed` + stable transcript key + fixed
    keys for streaming/status items; pure `picoTranscriptKey` helper.
  - test `PicoTranscriptKeyTest.kt` (new) — 4 tests (key shape, uniqueness,
    stability across content change, LazyColumn-uses-keys source pin).
- Tests: +4, -0, flipped 0.
- Behavioural delta: pico transcript keeps scroll position across snapshot
  replacement instead of jumping mid-stream.

## Embedded artefacts

- None. Pure key helper unit-tested + keyed-LazyColumn source-pinned; visual
  scroll smoothness confirms via emulator when the environment is stable.

## Operator-takeaway

Closes the loop with md2-0's FFI: no PicoAgentView hook needed (full reduced
snapshots drop into the existing snapshot+isStreaming contract), and this
keys-only pass makes the view scroll-stable under per-tick replacement — so when
the FfiPicoSessionSource lands, live pico renders smoothly. Pairs with the landed
pico UX trio (chips/states/thinking) for Harry's beautiful-pico-chat directive.
