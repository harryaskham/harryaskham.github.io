# Summary — bd-179115 flicker slice (cross-surface shared-image alias gate)

## Goal
Operator reopened the TUI graphics flicker P1 as **bd-179115** ("Fix TUI graphics
flicker and missing chat/navigation backgrounds", `operator-report` label),
assigned to po4-3, after the visual confirm showed flicker **persists** despite
prior fixes. New operator detail named three distinct symptoms:
1. Graphics identical except for screen placement still flicker in/out
   ("image upload IDs and virtual placements may still be mixed up").
2. Chat bubbles render incorrectly in **non-kitty** TUI mode.
3. Nav subpanels render no background **except one panel**.

This slice lands the highest-confidence fix for symptom 1.

## Root cause (symptom 1, default sharing-off config)
`SurfaceManager::mark_retained` (crates/caco-tui/src/kitty.rs) was
**unconditionally** inserting into `shared_retained_images` — the
`data_hash -> image_id` cross-surface alias map — even when
`cross_surface_retained_image_sharing` is `false` (the real-app default after
`apply_graphics_config`, config key `cross_surface_image_sharing` default false).

The display read path (`retained_image_id`) is already flag-gated, but the
delete/cleanup guards (`other_surface_displays_image` sibling checks,
`remove_shared_retained_image_if_unbacked`) consult the alias map whenever it is
non-empty. With sharing off, two identical-content surfaces at different screen
positions overwrote each other's `data_hash -> image_id` alias, so a later
cleanup could mis-track which image backs that hash and retire an image a sibling
still displayed — visible as flicker for graphics identical except for placement.

## Fix
Gate the `shared_retained_images.insert` on `cross_surface_retained_image_sharing`
so the map stays empty in the default config, making the `is_empty()`-guarded
consult sites correctly no-op. No behavior change when sharing is intentionally
enabled.

## Tests
- New: `mark_retained_skips_shared_alias_when_cross_surface_sharing_disabled_bd_179115`
  — sharing disabled via `apply_graphics_config`, two byte-identical surfaces at
  distinct rects marked retained, asserts `shared_retained_images` stays empty and
  per-surface retention still resolves each surface's own image id.
- Regression: 33 shared/* + 44 retained/* caco-tui lib tests green; full caco-tui
  lib suite via the test queue.

## Scope / remaining
This is a **partial** fix for bd-179115 (placement/flicker slice only). bd-179115
stays open; remaining symptoms — non-kitty chat-bubble rendering and nav-subpanel
backgrounds — need further investigation and will be decomposed into focused
slices. Localization so far: subpanel backgrounds DO resolve a gradient style
(both `sidebar` and `sidebar_panel` import `fx.yaml` with `background_style:
gradient`) and each subpanel is keyed distinctly (`enh:background:{panel_id}`), so
the "except one panel" cause is in the Rust render/retire path, not theme config.

## Diff
See the landed squash commit recorded in the reintegration receipt (code commit
`d4be4dea3` on the agent branch touching only `crates/caco-tui/src/kitty.rs`).
