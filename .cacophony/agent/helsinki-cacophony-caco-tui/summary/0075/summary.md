# bd-3cdaab: avoid stale-surface sweep scan when no TUI Kitty surfaces exist

## What changed

- `SurfaceManager::retire_stale_surfaces_for_current_redraw()` now has an explicit `self.surfaces.is_empty()` fast path.
- Added regression coverage so no-surface/text-mode/early-startup frames keep the explicit fast path.

## Why

The stale-surface sweep is a graphics correctness backstop. ASCII/no-graphics and early startup frames usually have no Kitty surfaces at all; they should return immediately without depending solely on the live-count equality shortcut. This keeps graphics bookkeeping overhead closer to the text TUI when no surfaces are active and guards against future regressions that could reintroduce an empty map sweep.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_3cdaab"` — `tj-ab8e9671`, passed
