# Session summary — bd-8e64f0: md-body + pico-body-md inline-code overflow (bd-d8a9f8 follow-on)

## Goal

Immediately after root-causing + fixing the chat-body 966px overflow (bd-d8a9f8),
proactively check whether the SAME global root — `:not(pre) > code { white-space:
nowrap }` defeating overflow-wrap — caused the same latent inline-code overflow in
the OTHER markdown render contexts. It did: `.md-body` (bead descriptions, agent
goals, SUMMARIES) and `.pico-body-md` (pico markdown) both inherited the global
nowrap with no override. Fix them the same scoped way.

## Bead(s)

- `bd-8e64f0` — caco-web: md-body + pico-body-md inline-code overflow (filed +
  claimed + closed this session). Follow-on of bd-d8a9f8 (chat-body), same root.

## Before state

- `.md-body code` and `.pico-body-md code` had NO white-space override, inheriting
  the global `:not(pre) > code` nowrap. Live-DOM probe (real renderMarkdown + CSS
  at 320px): md-body code w=1127 ws=nowrap overflow=807; pico-body-md code w=1207
  ws=nowrap overflow=887. So a long inline command in a bead description / agent
  goal / summary / pico markdown overflowed horizontally on narrow widths.

## After state

- Added `white-space: pre-wrap; overflow-wrap: break-word;` to `.md-body code` and
  `.pico-body-md code` (overriding the global nowrap), with short `<code
  class=bead-ref>` bd-id chips kept nowrap (intact). Re-probe: md-body overflow
  807->0, pico-body-md 887->0, chat-body still 0. All three inline-code contexts
  now wrap. Needle guard added.

## Diff summary

- Code commit: pending (final landed squash SHA from the reintegration receipt).
- Files: crates/caco-web/static/style.css (.md-body code + .pico-body-md code wrap
  + bead-ref nowrap), crates/caco-web/src/tests.rs (needle guard). CSS-only, no
  app.js/wasm change.
- Tests: +1 needle guard (md_and_pico_inline_code_wrap_not_nowrap_bd_8e64f0).

## Operator-takeaway

A latent-bug sweep paid off: the bd-d8a9f8 root cause (a global inline-code nowrap
defeating overflow-wrap) was not chat-specific — it silently affected every
markdown render context that didn't override it, including SUMMARIES and bead
descriptions. The same live-DOM computed-style probe confirmed md-body +
pico-body-md overflow and the fix in one pass. When you fix a global-rule trap,
sweep the sibling render contexts for the same latent manifestation.
