# Session summary — bd-3a7bd9: typed rich tool-result card coverage (Inbox/BeadList/AgentList)

## Goal

Live-test the typed rich tool-result cards — a native-parity feature the web
renders (renderPicoRichCard: Inbox/BeadList/AgentList) but that had no live
assertion (the main scenario's tool is plain text), the same rendered-but-untested
pattern that hid the bd-b357f0 widget_placements stale-wasm bug.

## Bead(s)

- `bd-3a7bd9` — live coverage for typed rich tool-result cards (Inbox/BeadList/AgentList)

## Before state

- Failing tests: none. The three RichResult card kinds were rendered by the web
  but never asserted end-to-end; a stale wasm or render regression would pass.

## After state

- Failing tests: none. New live subscenario serves a snapshot with three Tool
  items carrying rich Inbox / BeadList / AgentList payloads and asserts all three
  cards render: inbox head "inbox · 2 messages" + body, bead row id+title+status,
  agent row id+state, through the real ws -> wasm -> DOM path. 2/2 clean —
  confirms the wasm carries the Tool `rich` field and the cards render correctly.
- Vision pass confirmed the cards are polished, consistent, and native-app-like
  (aligned keys/values, right-flush status tags, unified design language).
- caco-web bin 12; `--lib` 651; clippy clean.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-web/src/bin/caco-web-observe.rs` — rich-cards mock + subscenario + eval + screenshot.
- Tests: +1 live subscenario.
- Behavioural delta: test-only (parity coverage); no product change.

## Embedded artefacts

- `web/screenshots/rich-cards.png` — the three typed cards.

## Operator-takeaway

Closes the typed-rich-card coverage gap: all three RichResult card kinds
(Inbox/BeadList/AgentList) now have an end-to-end live assertion, matching the
caco-tui/native typed tool-result rendering and protected against the stale-wasm
silently-breakable pattern.
