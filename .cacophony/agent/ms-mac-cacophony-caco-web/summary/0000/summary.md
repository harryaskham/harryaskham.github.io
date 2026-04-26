# Session summary — unclipped narrow Status hero

## Goal

Run the caco-web active duty cycle, inspect the rendered dashboard with lightweight Playwright evidence, and fix the focused visual defect found: the Status page hero collapsed into a clipped strip on narrow mobile-width viewports.

## Bead(s)

- `bd-8c4e7e` — caco-web Status hero is clipped on narrow viewports

## Before state

- Failing tests: none at cycle start.
- Relevant metrics: no assigned in-progress caco-web beads and no ready open beads for `caco-web`, `dashboard`, `web`, `browser`, `workspace`, `playwright`, `webui`, `summaries`, or `visual-polish`.
- Context: current-assets Playwright observation against caco-web `v1.2.565` found `.status-hero` at 390x844 rendered as a 49px/53px-tall strip while its `scrollHeight` was 386px/328px and `overflowY` was `hidden`. The live-orchestration copy and quick facts were clipped in the screenshot.
- Evidence: `/tmp/caco-web-duty-visual-224128-observation.log`, `/tmp/caco-web-status-narrow-224246-observation.log`, `.playwright-cli/page-2026-04-26T21-43-00-909Z.png`.

## After state

- Failing tests: none observed.
- Relevant metrics: Playwright validation at 390x844 reports `.status-hero` `h=330`, `sh=328`, `flexShrink="0"`, `clipped=false`; `#cluster-pulse-expand-btn` remains `position="absolute"`; console stayed `0` errors and `0` warnings.
- Context: the Status hero now keeps its mobile content visible and the page scrolls instead of compressing the hero into a hidden-overflow strip.
- Evidence: `/tmp/caco-web-bd-8c4e7e-final-224828-validation.log`, `.playwright-cli/page-2026-04-26T21-48-44-312Z.png`.

## Diff summary

- Commits: `c3d7f5126` (`bd-8c4e7e: prevent narrow status hero clipping`).
- Files touched: `crates/caco-web/static/style.css`, `crates/caco-web/src/tests.rs`.
- Tests: added `style_css_keeps_status_hero_expand_button_absolute_after_tooltip_rule_bd_8c4e7e`.
- Behavioural delta: `.status-hero` now has `flex-shrink: 0` so the vertical Status view does not compress it on short/narrow screens, and the tooltip host override keeps the cluster-pulse expand button absolutely positioned after the generic `[data-tooltip]` rule.
- Validation: `git diff --check`; `cargo fmt --all -- --check`; focused regression test; `CARGO_BUILD_JOBS=2 cargo build -p caco-web --bin caco-web-dev-server`; Playwright proof; `CARGO_BUILD_JOBS=2 cargo check -p caco-web --all-targets`; `CARGO_BUILD_JOBS=2 cargo test -p caco-web --lib` (292 passed); post-rebase focused regression rerun passed.

## Operator-takeaway

The Status dashboard now looks trustworthy on mobile width again: the live-orchestration hero expands to show its copy and backend/snapshot facts instead of clipping almost all of its content into a thin decorative strip.
