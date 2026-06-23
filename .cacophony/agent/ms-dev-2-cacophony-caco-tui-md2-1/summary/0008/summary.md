# Session summary — caco-tui profile gate-discipline (mirror caco-web)

## Goal
Add a durable interim validation discipline to the shared caco-tui profile (run the FULL cargo test -p caco-tui --lib before every caco-tui land), mirroring the caco-web profile's, per caco-ctrl's mirror-request after tonight's gate-blocks.

## Bead(s)
- No implementation bead — controller mirror-request (profile/operational maintenance). Context: the speech_popup gate-block bd-475bce (from bd-6ab0e3) I fixed earlier this session; durable fix bd-ff92cd / bd-d818db.

## Before state
- caco-tui profile Validation used a focused TUI_TEST_FILTER, which can MISS sibling-test assertion drift (what speech_popup was).

## After state
- caco-tui profile now carries the gate discipline: ALWAYS run the FULL cargo test -p caco-tui --lib before landing any caco-tui change, with the speech_popup example + test-small/echo-gate rationale. Propagates to all caco-tui agents on refresh.

## Diff summary
- Files touched: .cacophony/profiles/caco-tui.md (one Gate-discipline paragraph). Doc/process only; no code change.

## Operator-takeaway
caco-tui (like caco-web) is in test-small, so a UI-edit assertion drift passes the echo reint-gate yet re-blocks every cacophony-fast-tests agent's reints. The focused-filter habit is the trap. Full-crate --lib before landing is the interim guard until the daemon-side gate re-enable (bd-ff92cd/bd-d818db).
