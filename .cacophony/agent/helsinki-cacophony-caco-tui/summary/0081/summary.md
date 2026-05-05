# bd-0a1beb: skip TUI animation phase checks when no animations are active

## What changed

- `TuiEvent::AnimationFrame` handling now short-circuits before calling `BorderIntegration::animation_phase_would_advance()` when:
  - `graphics_active_animation_only` is enabled, and
  - no redraw-driven animated Kitty surfaces are active.
- Active-animation behavior is preserved: when animated surfaces are active, the phase gate still decides whether to redraw.
- `activeAnimationOnly = false` behavior is preserved: the handler still checks phase advancement and allows idle redraw behavior according to the existing logic.
- Added regression coverage for the handler ordering.

## Why

When active-animation-only mode is enabled and no animated Kitty surfaces are visible, an animation frame cannot produce a graphics redraw. Computing whether the border/background phase would advance is therefore unnecessary. Skipping that check trims idle animation wake overhead and keeps the graphics path closer to ASCII/text behavior when no graphics animations are active.

## Validation

- First targeted run caught the source assertion matching the event-batching helper instead of the real handler.
- Second targeted run caught bad source-slice bounds.
- Fixed the regression test to anchor on the real handler body.
- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_0a1beb"` — `tj-de344aef`, passed
