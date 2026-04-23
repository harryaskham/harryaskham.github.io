# Session summary — bd-7b55dd P0: speech_popup row_count off-by-one fix

## Goal

Unblock release cuts. update-helper trying to cut v1.2.528 hit two
failing assertions in `cargo test -p caco-tui --lib`. P0 release-
blocker by labelling.

## Bead(s)

- `bd-7b55dd` — test-small failure: speech_popup row_count
  assertions off-by-one (blocks releases)

## Before state

`bd-db10da` (landed earlier today) added a new 'STT Indicator Dot'
row to the speech popup that toggles the persistent dot visibility,
but the row_count assertions in `speech_popup.rs:577 + 586` were
not updated. Failures:

    row_count_with_capabilities          left: 10, right: 9
    row_count_with_local_device_routing  left: 12, right: 11

Both off-by-exactly-one matching the new row insertion.

## After state

- `row_count_with_capabilities`: expected 9 → 10. Inline budget
  comment now reads `STT: input_mute, stt_indicator_dot
  (bd-db10da), stt_model, input_routing = 4`.
- `row_count_with_local_device_routing`: expected 11 → 12 (still
  +2 for the device rows; +1 for the new dot).
- `row_count_without_capabilities` floor: 9 → 10 (the dot row is
  emitted unconditionally so the floor moves with it).

## Diff summary

- `crates/caco-tui/src/views/speech_popup.rs`: 5 insertions / 5
  deletions in the test mod. No production-code change.
- `cargo test -p caco-tui --lib speech_popup`: 14/14 pass.

## Embedded artefacts

(none)

## Operator-takeaway

Trivial fix; shipped immediately because v1.2.528 cut was being
held. Pattern reminder for future contributors touching
`setting_rows`: any new row needs the row_count test budget
updated and a one-line comment naming the originating bead so the
budget tracks against intent rather than just count.
