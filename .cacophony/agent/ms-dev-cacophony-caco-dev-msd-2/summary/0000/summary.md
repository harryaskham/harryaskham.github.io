## Goal
Extend bd-00d486 scratch-buffer kitty optimization to animation-frame emitter.
## Bead(s)
- bd-a98d92
## Before state
append_animation_frame_command used per-chunk format!() String allocs.
## After state
Reuses scratch Vec<u8> + write! direct. Byte-identical output via new test (8 cases). cargo test-small green, clippy clean.
## Diff summary
crates/caco-tui/src/kitty.rs: rewritten emitter + legacy ref + test.
## Operator-takeaway
Completes kitty emitter optimization sweep from bd-00d486.
