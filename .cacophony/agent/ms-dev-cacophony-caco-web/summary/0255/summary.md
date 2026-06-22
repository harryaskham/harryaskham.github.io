# Session summary — bd-02730e: Pico honors widget_placements (native parity)

## Goal

Close a native-parity gap: the shared `caco-picophony` snapshot carries
`widget_placements` (bd-eb071d) — per-widget `aboveEditor`/`belowEditor` intent
explicitly meant "for tui/macos/web by placement" — but the web Pico renderer
ignored it entirely, dumping every widget flat in the footer. Route widgets by
their placement hint like the TUI/macOS clients.

## Bead(s)

- `bd-02730e` — caco-web Pico: honor widget_placements (belowEditor widgets below composer)

## Before state

- Failing tests: none.
- `renderPicoFooter` rendered all `widgets` in the footer; `widget_placements`
  had zero usage in app.js.

## After state

- Failing tests: none.
- `renderPicoFooter` reads `snap.widget_placements` and routes each widget:
  `belowEditor` → a new region beneath the composer (`#agent-pico-below`),
  `aboveEditor`/no-hint → footer (unchanged). The below region is hidden when
  empty, so existing sessions are unaffected. Added the `#agent-pico-below` div +
  `.agent-pico-below` CSS.
- caco-web `--lib` 649 (new static guard); clippy clean (all-targets).

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-web/static/app.js` — placement-aware widget routing + below div.
  - `crates/caco-web/static/style.css` — `.agent-pico-below`.
  - `crates/caco-web/src/tests.rs` — guard
    `pico_widgets_honor_placement_intent_bd_02730e`.
- Tests: +1 static source guard.
- Behavioural delta: belowEditor-hinted widgets now render beneath the composer.

## Operator-takeaway

A live Playwright subscenario for this was prototyped and passed locally, but the
shared main `pico-pane` scenario is currently flaky under load (the
order-dependent steer-while-streaming vs prompt-after-streaming timing — filed
separately), so the deterministic check is a static source guard (matching the
bd-7e561e pattern). The widget-placement read is the structural parity win; the
live subscenario can be re-added once the main scenario is made deterministic.
