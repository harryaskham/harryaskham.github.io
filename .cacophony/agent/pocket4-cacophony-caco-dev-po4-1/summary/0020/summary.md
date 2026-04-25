# Session summary — bd-45c6fb remove third-party font loads from Pages

## Goal

Remove third-party telemetry from the public GitHub Pages site by
stripping the active Google Fonts loads on every docs/*.html page.

## Bead(s)

- `bd-45c6fb` — [docs] Self-host or remove third-party font loads on Pages (P3 bug, gh-pages-privacy-audit)

## Before state

- 23 `docs/*.html` pages each loaded:
  ```
  <link rel="preconnect" href="https://fonts.googleapis.com">
  <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
  <link href="https://fonts.googleapis.com/css2?family=Inter:...&family=JetBrains+Mono:...&display=swap" rel="stylesheet">
  ```
  on every visit, leaking visitor IP / User-Agent / Referer to a
  third party (Google).
- The previous parity contract pinned the load.

## After state

- All three Google Fonts links removed from all 23 docs/*.html pages.
- The CSS font stack in `docs/style.css` falls back gracefully:
  - `--font-sans: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif`
  - `--font-mono: 'JetBrains Mono', 'Fira Code', 'FiraCode Nerd Font', 'SF Mono', monospace`
- Visitors with Inter / JetBrains Mono installed get them locally;
  everyone else gets the system UI/mono stack. No active third-
  party request leaves the Pages origin.

## Diff summary

- Commit: 051bb3557
- Files touched: 23 docs/*.html (-3 lines each) + 1 test addition
  in `crates/caco-web/src/tests.rs` (+58, -0).
- New `docs_html_pages_have_no_third_party_font_loads_bd_45c6fb`
  walks every docs/*.html and asserts no `fonts.googleapis.com` /
  `fonts.gstatic.com` strings remain.
- cargo test-small: 264/264 pass.

## Scope note

`crates/caco-web/static/index.html` still has the same
Google-Fonts load. That's a different surface (private webapp, not
the public Pages site that the privacy audit flagged) and is left
to a follow-up bead — the audit scope was Pages.

## Operator-takeaway

Pages now stops phoning home to Google on every visit. CSS font
stack handles the visual fallback. The new test prevents drive-by
reintroduction of third-party font loads in the public docs HTML.
