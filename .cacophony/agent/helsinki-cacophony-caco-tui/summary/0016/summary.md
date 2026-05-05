# bd-d0b84a: skip full stale-surface sweep when all graphics surfaces are live

## What changed

- Added `SurfaceManager::current_redraw_live_count`, reset at `begin_redraw()`.
- Increment the counter the first time each registered surface is touched during a redraw (`reserve`, `reserve_without_resize_delete`, `mark_live`, refresh paths, and new reservations).
- `retire_stale_surfaces_for_current_redraw()` now fast-returns when every registered surface was touched, avoiding a full `HashMap` scan on steady cached graphics frames.
- Added regression coverage for the all-live fast path.

## Why

The bd-102f71 stale-placement sweep is important correctness protection for view/tab/workspace transitions, but the common cached graphics path touches every active surface during render. In that steady-state case, a second full scan over the surface map cannot find stale entries. Counting unique live touches lets the sweep preserve correctness while avoiding extra O(n) work every frame.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_d0b84a"` — `tj-55e96cb9`, passed
