# Session summary — bd-5e2a50 TUI fullscreen coverage settings

## Goal

Add three TUI config options that let operators control whether
fullscreen mode on a panel also covers the global sidebar, header
(tab bar) and footer (status bar) rows. Defaults to "cover everything"
so fullscreen is genuinely distraction-free out of the box, while
preserving the ability to keep any of the three chrome rows visible.

## Bead(s)

- `bd-5e2a50` — Add TUI fullscreen config options with coverage settings

## Before state

- Failing tests: none.
- Relevant metrics: TUI fullscreen mode (toggled via Ctrl-Shift-F /
  the per-panel fullscreen action) only suppressed inter-tile splits
  inside the workspace area, but always kept the header tab bar, the
  navigation sidebar and the bottom status bar drawn (per the
  bd-eaefe4 + bd-101500 layout). Operators couldn't get a true
  full-terminal panel view without leaving the TUI.
- Context: the bead spec says all three options default to `true`
  (cover everything). That changes default behaviour, so the new
  config field gates it: with no config block, fullscreen still
  becomes fully-immersive (covers all three); with explicit
  `cover_*: false` flags, individual chrome rows can be preserved.

## After state

- Failing tests: none. `cargo test -p caco-config --test config
  fullscreen` reports 5 passed (5 new). `cargo test -p caco-tui --lib`
  reports 2915 passed, no regressions. `cargo test-small` reports
  183 passed.
- Relevant metrics: new `caco_config::FullscreenConfig` struct exposes
  `cover_sidebar / cover_header / cover_footer` (all `Option<bool>`
  defaulting to `true`), plus an `overlay()` and three `covers_*()`
  helpers. `TuiConfig` gains a `fullscreen: Option<FullscreenConfig>`
  field that participates in the existing global → node overlay merge.
  `App::render` consults `stored_tui_config.fullscreen` and resolves
  per-flag header/sidebar/footer coverage; rows that are covered get
  zero-height layout constraints and skip their render call entirely.
- Context: bead ACs satisfied. AC1 (all three boolean defaults to
  true): asserted in `fullscreen_config_defaults_all_coverage_to_true`
  and threaded through render via `cfg.map(...).unwrap_or(true)`.
  AC2 (documented): rustdoc on the struct + each field includes the
  bd-id and a YAML example. AC3 (fullscreen respects settings):
  exercised via the `tui_config_overlay_merges_all_fields` test (now
  also includes `fullscreen: None` slot) and through the App render
  path that uses `header_height: u16` / `footer_height: u16` /
  `fs_cover_sidebar: bool` to gate layout constraints.

## Diff summary

- Files touched:
  - `crates/caco-config/src/model.rs`:
    - New `FullscreenConfig` struct (3 fields + 4 methods, ~50 lines)
      placed before `BottomPanelConfig` to keep the panel-related
      configs together.
    - `TuiConfig.fullscreen` new field (Option<FullscreenConfig>).
    - `TuiConfig::overlay()` extended to merge the new field with the
      same global-then-node pattern used for sub-structs.
  - `crates/caco-config/tests/config.rs`:
    - 5 new unit tests for FullscreenConfig (defaults, explicit-false,
      overlay, YAML round-trip, absent block).
    - `fullscreen: None` added to 6 existing TuiConfig literals so
      the workspace still builds.
  - `crates/caco-tui/src/app.rs`:
    - `render()`: new `(fs_cover_header, fs_cover_sidebar,
      fs_cover_footer)` tuple resolved from stored_tui_config.
    - Layout constraints use `header_height` / `footer_height`
      (0 when covered, 1 otherwise) for both bottom-panel-full-width
      and normal layout branches.
    - `nav_hidden || fs_cover_sidebar` short-circuits the
      sidebar-included workspace split.
    - Tab bar render and status bar render are guarded by
      `if header_height > 0 { ... }` / `if footer_height > 0 { ... }`
      so they truly disappear instead of drawing zero-height rows.
- Tests: +5 unit tests in caco-config; existing 6 caco-config TuiConfig
  literal sites updated.

## Embedded artefacts

(None — pure code change.)

## Operator-takeaway

`tui.fullscreen.cover_{sidebar,header,footer}: false` opts out of the
new immersive default per chrome row. The bead's AC defaults are now
load-bearing: operators who liked the old "fullscreen still shows the
sidebar/tab bar/status bar" behaviour need to set all three to `false`
explicitly. Worth a follow-up bead if we want a fourth `mode: legacy`
top-level switch, but the current shape mirrors `tui.bottom_panel`
and `tui.sidebar` exactly so operators familiar with those will find
it intuitive. Single render-path commit, no cross-crate API churn,
clippy clean.
