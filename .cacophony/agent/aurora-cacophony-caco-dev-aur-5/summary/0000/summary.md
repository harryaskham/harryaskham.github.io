# Session summary — Re-enable graphics on the default 'high' theme (bd-664462)

## Goal

Resolve the P1 operator-reported regression where every TUI dropped to text
mode (no Kitty/Ghostty bitmap graphics) at steady state after a recent update.
The bead had been closed once with no visual proof and reopened, with two prior
investigation cycles chasing a kitty.rs deferred-graphics-frame theory that kept
coming up empty. The real goal was to find what actually keeps graphics off at
steady state — in any terminal — and fix it.

## Bead(s)

- `bd-664462` — Regression: TUI bitmap graphics dropped to text mode in
  Kitty/Ghostty after bd-cc917a deferred-graphics-frame landed (P1 bug)
- `bd-f173e4` — (draft, filed via reflect-session) Guardrail: flag
  reintegrations that change operator-facing theme/globals config under an
  unrelated bead

## Before state

- Failing tests: none in caco-config; the live symptom was not test-covered.
- Operator (Harry) on v1.2.1016: Kitty/Ghostty bitmaps render as text mode at
  steady state on every TUI surface.
- `.cacophony/themes/high.yaml` had `graphics.enabled: false`. `high` is the
  default active theme (`globals.theme: high`, `tui.theme_name: {{ globals.theme }}`).
- `effective_graphics()` resolves the active theme's graphics block, so
  `config_disabled` became true and `is_graphics_capable()` returned false
  regardless of terminal capability detection.
- `high.yaml` was the ONLY fidelity theme setting `enabled: false`; ultra,
  medium, low, default, perf all inherit `enabled: true` from default.yaml.

## After state

- Failing tests: none. New regression test passes
  (`repository_default_theme_keeps_graphics_enabled_bd_664462`), and the
  existing theme-resolution suite (8 tests) is green.
- `high.yaml` now sets `enabled: true` (with a comment explaining why it must
  stay on); animation fidelity tuning (animations/background_animate off,
  render_scale 2, format rgba) is intentional and preserved.
- The active default theme resolves graphics-enabled, so the live TUI no longer
  suppresses bitmaps at steady state.

## Diff summary

- Code/content commits: 4ba0f024d5 (final landed squash SHA from reintegration
  receipt).
- Summary artefact commit: intentionally omitted.
- Files touched: `.cacophony/themes/high.yaml`,
  `crates/caco-config/tests/config.rs`.
- Tests: +1 (`repository_default_theme_keeps_graphics_enabled_bd_664462`,
  loads the real repo config and asserts the active theme resolves graphics
  enabled).
- Behavioural delta: bitmap graphics restored on the default theme; root cause
  was config/data (a stray `enabled: false` from unrelated caco-web
  reintegration 44f6dce456 / bd-4b1949), not kitty.rs defer logic.

## Embedded artefacts

None. This session is under tmux with `TERM_PROGRAM=tmux` and no Kitty/Ghostty
display backend, so a live visual capture cannot be produced here. The fix is
config-level and proven by config-resolution test, but the bead's hard
requirement for an on-terminal capture must be satisfied by an agent on a real
Kitty/Ghostty host (e.g. the kitty.rs graphics owner msm-2 on ms-mac) before
final close.

## Operator-takeaway

The "all graphics broke" outage was never a kitty source bug — it was one stray
YAML line (`graphics.enabled: false`) added to the default `high` theme by an
unrelated caco-web reintegration, which silently disabled bitmaps on every TUI.
The bead's framing sent two prior cycles chasing the wrong (code) layer. Fixed
and now guarded by a test that fails if the active default theme disables
graphics. The remaining open question is the class problem: stray
operator-facing config edits riding unrelated beads ship green with no review
gate — filed as draft bd-f173e4.
