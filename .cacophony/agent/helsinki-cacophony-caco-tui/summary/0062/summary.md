# bd-d81bad: avoid duplicate active-placement lookup when releasing TUI Kitty images

## What changed

- `SurfaceManager::decrement_active_placement()` now returns whether any active placements remain for the image after decrementing.
- `release_displayed_surface_image()` uses that return value instead of probing `active_image_placements.contains_key(image_id)` again after decrementing.
- Added regression coverage guarding against the old double-probe shape.

## Why

Releasing a displayed retained/shared Kitty image is on the graphics lifecycle path for retire/resize/replacement. The previous code checked the active placement map, decremented it, and then checked the same map again. Returning the post-decrement state removes one avoidable `HashMap` lookup while preserving placement-delete vs full-image-delete behavior.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_d81bad"`
  - first attempt hit transient daemon reachability
  - retry `tj-ddca3d5f`, passed
