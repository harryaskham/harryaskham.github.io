# Session summary — Fix tabbed-panel body background (bd-c895cf)

## Goal

A TUI rendering bug: in the `high` theme, tabbed panels rendered their whole
body with the opaque-light tab-strip background instead of the translucent
panel surface, while non-tabbed panes looked correct. The goal was to fix the
tabbed-container render path so a tabbed panel's body matches a non-tabbed
panel body (translucent), with only the tab pills using the opaque-light
styling.

## Bead(s)

- `bd-c895cf` — Tabbed panel body renders opaque-light panel_tabs background
  instead of translucent panel_style (P2 bug, caco-tui)
- Filed (not claimed) `bd-8eaa06` — Non-tabbed panels (agent detail, chat)
  render without panel borders: a distinct missing-borders symptom surfaced
  by config-helper/operator during triage, split out to keep this fix atomic.

## Before state

- Failing tests: none (the bug was visual; one existing test
  `pane_tabs_record_panel_tabs_role` actually encoded the buggy behavior).
- Symptom: tabbed panels showed an opaque light body (panel_tabs opacity
  0.82-1.0); non-tabbed panes were correctly translucent (panel 0.18/0.28).
- Root cause: `views/pane_tabs.rs` recorded one `record_graphics_panel` over
  the whole panel `area` with `PanelRole::PanelTabs` in both the horizontal
  (~line 184) and vertical (~line 377) tab paths.

## After state

- Failing tests: none.
- caco-tui lib suite: 4140 passed / 0 failed; clippy `-D warnings` clean;
  rustfmt clean.
- Tabbed panel body now records `PanelRole::Panel` (translucent), matching
  non-tabbed panes; tab pills keep panel_tabs styling via their own
  `record_span_pill` calls; `top_gap` border registration unchanged.
- Peer-reviewed and ACKed by ms-mac caco-dev-msm-2 (TUI/graphics lane owner).

## Diff summary

- Code commit: 22e91bfd7 (final landed squash SHA from reintegration receipt).
- Files touched: `crates/caco-tui/src/views/pane_tabs.rs` (one file, +65/-6).
- Tests: rewrote `pane_tabs_body_records_panel_role_bd_c895cf` to assert the
  body uses `Panel`; added
  `suppressed_pane_tabs_only_keeps_translucent_body_panel_bd_c895cf`; corrected
  `suppressed_pane_tabs_keep_ascii_border_without_graphics_request` to mask
  both Panel and PanelTabs for the full ASCII fallback. Net +2 tests.
- Behavioural delta: tabbed panel bodies render translucent (panel_style) like
  non-tabbed panes; only the tab pills remain opaque-light.

## Operator-takeaway

The opaque-light tabbed-panel body was a single mis-assigned render role
(`PanelTabs` vs `Panel`) in the tabbed-container draw path, not a theme or
compositing-clipping problem — the per-request `role` selects the background
profile and the `top_gap` keeps the border strip, so flipping the body to
`Panel` fixes it cleanly. A separate, still-open symptom (non-tabbed
agent-detail/chat panes missing borders, bd-8eaa06) lives in a different
path (PanelBorder + effective_panel_border_visibility default) and was
deliberately not folded into this fix.
