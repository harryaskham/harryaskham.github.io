# Session summary — caco-web: notifications break long tokens on mobile (bd-54f071)

## Goal

With the daemon recovered from the spike backpressure, I ran the repo's
daemon-backed caco-web-observe comprehensive pass (the correct live-observation
tool — raw chromium --screenshot hangs on the live SPA's SSE). Across 16 routes +
21 Workspace pane types it confirmed the dashboard is console-clean, and the
narrow-viewport overflow probe surfaced a real mobile defect: notifications
overflow horizontally when they contain long unbreakable tokens. This session
fixes that.

## Bead(s)

- `bd-54f071` — caco-web: notification body/title overflow horizontally on mobile (missing overflow-wrap) (filed + claimed + fixed this session)

## Before state

- Failing tests: none.
- caco-web-observe overflow probe (narrow 390px, real notification data):
  `.notification-body` w=221 scrollWidth=405 overflowX=visible; `.notification-item`
  w=348 sw=467; on notifications with long tokens (workflow file names, "cacophony
  run 27…" run IDs, URLs, agent IDs). `.notification-body` had `white-space:
  pre-wrap` but no overflow-wrap, so single long tokens didn't break.
- The pass also confirmed: **console-clean across all routes** (no errors/
  exceptions); network artifact unavailable was a `@playwright/cli` tooling
  limitation (bd-20258e), not a dashboard defect.

## After state

- Failing tests: none (`cargo test -p caco-web --lib`, tj-59b60f74, + new contract
  test `style_css_notification_body_breaks_long_tokens_bd_54f071`).
- `.notification-body` and `.notification-title` now set `overflow-wrap:
  break-word`, so long unbreakable tokens break instead of overflowing on
  narrow/mobile widths. Additive (overflow-wrap can only reduce overflow).

## Diff summary

- Code/content commit: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-web/static/style.css`: `overflow-wrap: break-word` on
    `.notification-body` + `.notification-title`.
  - `crates/caco-web/src/tests.rs`: +1 contract test (bd-54f071).
- Tests: +1 / -0 / flipped 0.
- Behavioural delta: notifications no longer overflow horizontally on mobile from
  long tokens; desktop unchanged; CSS-only, no app.js change (avoids the sibling's
  in-flight caco-web auth work). chat-message-content was also flagged but already
  has min-width:0 + .chat-body word-wrap (residual overflow from code/pre/link
  children = a separate, documented follow-up, not this bead).

## Embedded artefacts

- `web/audit.md` — caco-web-observe route + Workspace-pane inventory (16 routes, 21 panes).
- `web/overflow-evidence.txt` — the overflow-probe numbers behind this fix.

## Operator-takeaway

The first real daemon-backed observation pass of the session (post-recovery)
confirmed the dashboard is console-clean and surfaced one concrete mobile defect
(notification token overflow), now fixed CSS-only. The correct live-observation
tool is caco-web-observe (not raw chromium, which hangs on the SPA's SSE). A
follow-up daemon-backed observe pass can re-confirm notification-body sw≈w; the
chat-code-child overflow remains for a separate nuanced slice.
