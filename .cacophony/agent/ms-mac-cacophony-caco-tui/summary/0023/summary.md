# Session summary — Audio tools unselected local devices

## Goal

Continue the TUI improvement loop by aligning Audio tools local-device routing display with the speech popup when no local input or output device is selected.

## Bead(s)

- `bd-212884` — TUI Audio tools local-device mode should show none when unselected

## Before state

- Failing tests: none existing for unselected local-device rows in Audio tools.
- Relevant metrics: not a performance change.
- Context: when Output Routing or Input Routing was `Local Device` but `routing.device` was `None`, the speech popup showed `(none)`, while Audio tools omitted the Device row entirely.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: not applicable.
- Context: Audio tools now renders an explicit `Device: (none)` row for local output or input routing when no device is selected. Existing friendly-name plus raw-id formatting is preserved when a selected device exists.

## Diff summary

- Commits: `91c4c2dd6`
- Files touched: `crates/caco-tui/src/views/audio.rs`
- Tests: +2 new none-state regression tests / -0 / flipped 0; the local-device test subset now covers four states.
- Behavioural delta: no routing semantics changed; Audio tools now exposes local-device none state instead of silently omitting it.
- Validation:
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui audio_view_local_ --lib`

## Operator-takeaway

Audio tools now makes the local-device routing state explicit even when no concrete device is selected, matching the speech popup and avoiding an ambiguous missing row.
