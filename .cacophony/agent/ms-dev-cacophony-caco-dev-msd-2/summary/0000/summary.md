## Goal

Audit kitty graphics rendering hot paths in caco-tui for
behaviour-preserving speedups, then implement and validate one focused
optimization. Operator-filed P2 task to fold into ongoing TUI graphics
work after recent fullscreen hit-test changes.

## Bead(s)

- `bd-00d486` — Audit TUI kitty graphics rendering for
  behavior-preserving speedups.

## Before state

- `SurfaceManager::append_placement_command_with_image_and_placement`
  was the per-image, per-redraw kitty placement emitter. For each
  4 KB base64 chunk it called `format!()` to build a fresh `String`,
  immediately threw the `String` away through `append_escape_command`,
  then either copied to the output `Vec<u8>` (non-tmux) or byte-stuffed
  ESCs into it (tmux passthrough).
- A 16 KB image produces 6 chunks → 6 transient String allocations per
  placement. With ~30 placements per redraw on a populated agent
  detail view, that's ~180 heap allocs per frame just for the kitty
  envelope.
- No micro-bench coverage; the cost was invisible to existing
  test-small runs.

## After state

- The placement-command emitter now writes the per-chunk header
  directly into a single reusable `Vec<u8>` scratch buffer via
  `std::io::Write::write_fmt`, then `extend_from_slice`s the
  already-ASCII base64 chunk bytes and the terminator (`\x1b\\`). The
  scratch buffer is `clear()`-reused across chunks. Final emission
  copies once into the output buffer, or byte-stuffs once for tmux.
- Output `Vec` reservation is now a single up-front `reserve` based on
  the chunk count, sized to 2× for the tmux byte-stuffing path so
  later `extend_from_slice` calls do not realloc.
- Empty-data (a=p) path is unchanged — still uses `format!()` once;
  not on the hot path.
- All other emitters (`append_animation_frame_command`, no-data
  `a=p` etc.) are untouched in this bead; only the documented hot path.

## Diff summary

- `crates/caco-tui/src/kitty.rs`:
  - `append_placement_command_with_image_and_placement` rewritten to
    use a reused scratch buffer (no per-chunk `format!()` String
    allocation).
  - `legacy_append_placement_command_with_image_and_placement` added
    in tests as the byte-equivalence reference.
  - 2 new tests:
    - `placement_command_optimization_matches_legacy_byte_for_byte` —
      asserts byte-identical output across 16 cases: empty data,
      single-chunk, multi-chunk, tmux/non-tmux, z=0/non-zero,
      negative z, and 0x1B-payload edge case.
    - `placement_command_optimization_perf_smoke` (gated on
      `CACO_BENCH_KITTY=1`) — measures legacy vs optimized on 100
      redraws × 30 placements × 16 KB and asserts the optimized path
      is not >10% slower than legacy.
- Local measurement (release profile, 3000 placements × 16 KB):
  - legacy: 57.8 ms
  - optimized: 34.9 ms
  - **~40% faster on the placement emit path**.
- `cargo test-small` 297 passed in caco-tui (full workspace green).
- `cargo clippy -p caco-tui --all-targets -- -D warnings` clean.

## Embedded artefacts

(None — this is a code-only optimization; the perf number above is
from the gated `placement_command_optimization_perf_smoke` test.)

## Operator-takeaway

The kitty placement hot path is now ~40% faster on realistic image
loads, with byte-identical output on every covered case. The
optimization pattern (write headers via `write!` into a reused
`Vec<u8>` scratch buffer, then extend with the data slice once) is
worth applying to the remaining `format!()` emitters in `kitty.rs`
(animation frame, no-data placement, animation-start, gap commands)
in a follow-up if the operator wants a fuller sweep — those are
called less often per frame so were not bundled into this bead.
