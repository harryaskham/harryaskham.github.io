# Session summary — bd-a60b07 TUI border animation ignores --no-animation

## Goal

Fix the operator-reported regression where the TUI's rotating-gradient border
animation kept running/flickering even with `--no-animation`, `--theme low`
(`graphics.animations: false`), and `animation_overrides.borders: false`. The
config side was already correct — the caco-tui border render path simply never
consulted the animations-enabled gate for the rotating-gradient style.

## Bead(s)

- `bd-a60b07` — TUI border (rotating-gradient) animation ignores
  `--no-animation`, `--theme low`, and `animation_overrides.borders` — flickers.

## Before state

- `--no-animation` (`KittyManager::force_disable_animations`), theme
  `graphics.animations:false`, and per-element `animation_overrides.borders:false`
  all surface through `is_element_animations_enabled(Borders)`, and the border
  EFFECT + animation_phase were already gated on it — but the rotating-gradient
  was NOT. `border_renderer.rs` applies `apply_rotating_gradient` whenever
  `key.animation_style == RotatingGradient && key.effect != None`, and the
  `BorderCacheKey` was built with `animation_style: style.animation_style`
  (RotatingGradient from `fx.yaml`) with no animations-enabled gate. Net: borders
  kept animating/flickering with animations disabled (a regression).

## After state

- The border cache key's `animation_style` is now gated: when border animations
  are disabled, `RotatingGradient` is downgraded to `Pulse` (which renders
  statically here because `animation_phase` is already neutralized by the same
  gate). The rotating-gradient apply path is therefore skipped, and the cache key
  stays consistent with what is drawn (a toggle correctly re-renders).
- Implemented as a small pure helper `effective_border_animation_style(style,
  animations_enabled)` (mirrors the background renderer's
  `background_animation_style_key(style, animations_enabled)` precedent), threaded
  via a new `animations_enabled` arg into `build_render_plan`.
- Tests: new unit test
  `effective_border_animation_style_gates_rotating_gradient_bd_a60b07` passes
  (RotatingGradient passes through when enabled; downgraded to Pulse when
  disabled). `cargo test -p caco-tui --lib effective_border_animation_style` =
  1 passed (REAL 3m48s compile on ms-dev — not the ms-mac false-green the
  controller warned about); `cargo clippy -p caco-tui --tests` clean for the
  change (added `#[allow(clippy::too_many_arguments)]` on the render fn since the
  new arg made it 8/7).

## Diff summary

- Code commit: pending final squash SHA from the reintegration receipt.
- Files touched: `crates/caco-tui/src/border_integration.rs` (single file, +62/-3).
  - New `effective_border_animation_style` gate helper.
  - `build_render_plan` gains an `animations_enabled` param (+ `#[allow]`), passed
    at both runtime call sites; the `BorderCacheKey.animation_style` is gated.
  - 2 test call sites updated; 1 new unit test.
- Tests: +1 (gate logic). Behavioural delta: with border animations disabled, the
  rotating-gradient border no longer animates/flickers; static borders + glow/
  effect are preserved (the gate only downgrades the animation style, not effect).

## Operator-takeaway

`--no-animation` (and `--theme low` / `animation_overrides.borders:false`) now
actually stops the border rotating-gradient animation again — the renderer was
the one place that ignored the unified `is_element_animations_enabled(Borders)`
gate for the rotating-gradient style. Fix is a single-site cache-key gate so the
border draws statically without flicker while keeping glow/effect intact.
