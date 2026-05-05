# bd-363867: stop default graphics themes from redrawing without active animations

## What changed

- Changed `.cacophony/themes/perf.yaml` from `active_animation_only: false` to `active_animation_only: true`.
- Added enterprise theme contract coverage asserting that the default enterprise/perf overlay keeps active-animation-only gating enabled.

## Why

The default `enterprise` theme imports `perf.yaml`. That overlay had been overriding the Rust/default config and SPEC-documented behavior by setting `active_animation_only: false`. In `App::AnimationFrame`, that legacy setting can allow timer-driven redraws even when no animated kitty surfaces are active, making static graphics sessions spend target-FPS draw/upload-pass work for no visible change.

Keeping the shared perf overlay on `active_animation_only: true` preserves the intended behavior: graphics animation wakeups are skipped unless visible kitty surfaces are actually animated. Operators can still explicitly opt out in a diagnostic/local overlay when they need legacy always-wake behavior.

## Validation

- `rustfmt --edition 2021 --check crates/caco-config/src/lib.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-config enterprise_theme_is_registered_and_well_formed"` — `tj-ed9b9ef2`, passed

Note: an earlier broader filter `enterprise_theme_contract` produced a passing cargo invocation with zero matching tests (`tj-d071e8b3`), so it was replaced by the exact passing test above.
