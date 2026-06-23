# Session summary — fix caco-tui speech_popup broken-on-main (bd-475bce)

## Goal

Fix the caco-tui `views::speech_popup` broken-on-main test failures that were blocking the reintegration gate for all cacophony-fast-tests agents. Caught by msd-3 via bd-89088a validation and offered to the caco-tui specialists; taken as squarely my lane.

## Bead(s)

- `bd-475bce` — [broken-on-main] caco-tui speech_popup: 6 tests fail on main — bd-6ab0e3 agent-DM rows shifted row_count/activate_row assertions (gate-restore evidence, bd-ff92cd lineage)
- caused by `bd-6ab0e3` (4306e9701c, agent-DM TTS read-aloud + speed rows); same class as the closed `bd-7b55dd`

## Before state

- 6 failing tests on true main (test-small): row_count_with_capabilities, row_count_with_local_device_routing, activate_row_cycles_voice, activate_row_cycles_tts_model, activate_row_cycles_local_tts_speed_without_daemon, activate_row_toggles_input_mute
- Blocked the reint gate for every cacophony-fast-tests agent (the cargo test-small gate fails)

## After state

- Failing tests: none. cargo test -p caco-tui --lib speech_popup = 25 passed, 0 failed (validated via the queued path)
- Reint gate unblocked for cacophony-fast-tests agents

## Diff summary

- Code/content commit: 9622b8463 (final landed squash SHA from the reintegration receipt)
- Files touched: crates/caco-tui/src/views/speech_popup.rs (test-only)
- Tests: 6 flipped failing -> passing; counts/indices updated
- Behavioural delta: none (production code unchanged; only test assertions corrected)
- Key correction: bd-6ab0e3 pushes BOTH agent-DM rows UNCONDITIONALLY (the `available` field only greys them; setting_rows does not filter), so the row shift is +2, NOT the +1 msd-3 initially estimated. Verified by reading setting_rows = tui_tts_rows + stt_rows (no available-filter). Updated row_count 11->13 / 13->15 and activate_row indices +2 (tts_model 2->4, voice 3->5, tts_speed 5->7, input_mute 7->9).

## Operator-takeaway

The bd-6ab0e3 author added two UI rows but only updated the `available` field, not the test assertions — the recurring "insert-row-without-test-update" class (same as bd-7b55dd). The subtle trap: both rows are pushed unconditionally and `setting_rows` is unfiltered, so the shift is +2 even though one row (`agent_dm_speed`) is only interactable when read-aloud is on. A code-read (not a blind apply of the reporter's +1 estimate) was needed to get the correct shift; the queued test confirmed +2. The durable fix for this class remains the gate restore (bd-ff92cd) so these never land on main again.
