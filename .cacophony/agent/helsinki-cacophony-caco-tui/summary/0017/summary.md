# bd-c65cfa: gate TUI animation ticker while no animated graphics surfaces are active

## What changed

- Added an `animation_activity_tx` watch signal to `App`.
- The high-frequency animation task now parks when `activeAnimationOnly` is enabled and no visible kitty surface is currently marked animated.
- After each graphics render/upload pass, the app publishes whether animated surfaces are active so the ticker wakes only when animation work is actually visible.
- Added regression coverage that an active animated surface flips the activity signal and would unpark the ticker.

## Why

`activeAnimationOnly=true` previously prevented `AnimationFrame` events from forcing redraws when no animated surface was active, but the animation task still woke at the configured graphics FPS and sent/drop-processed events. Text/ASCII mode does not pay that idle timer cost. Parking the ticker removes those idle graphics wakeups while preserving smooth animation once a rendered surface becomes animated.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- First queued validation `tj-f7f4c53d` hit retryable `daemon_restart_recovered` infrastructure (no trustworthy result).
- Retry `tj-1568bfa8` caught a test-only missing `std::sync::Arc` qualification and was fixed.
- `caco test run --wait --command "cargo test -p caco-tui bd_c65cfa"` — `tj-9db1eaf6`, passed
