# Session summary — bd-2cf209: per-tool-type icons on Pico tool cards (TUI parity)

## Goal

Continue the render.rs-vs-web parity audit that found the notifications gap: the
web Pico tool cards showed a generic gear for every tool, while the TUI shows a
distinct per-tool-type icon.

## Bead(s)

- `bd-2cf209` — tool cards show a generic gear instead of the TUI's per-tool-type icon
- Mirrors shared `tool_type_icon` (`bd-6d607b`).

## Before state

- Failing tests: none. renderPicoItem's Tool branch hardcoded the gear glyph for
  every tool, losing the at-a-glance tool-type distinction (read vs bash vs git
  vs grep vs image) that the TUI gives via tool_type_icon.

## After state

- Failing tests: none. New picoToolTypeIcon(name) mirrors tool_type_icon exactly
  (read->document, bash->shell prompt, todo->checklist, edit/write->pencil,
  diff->plus-minus, git->branch, grep/find/search->magnifier, list->triple-bar,
  web/http/fetch->circled-plus, image/screenshot->framed-image, else->gear), used
  in the Tool card header. New live subscenario (6 tools of distinct families +
  unknown) asserts each card's expected glyph + >=5 distinct non-gear glyphs.
  2/2 clean; static guard pins the helper + the glyph set.
- caco-web bin 12; `--lib` 656 (+1 guard); clippy clean.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-web/static/app.js` — picoToolTypeIcon + Tool card uses it.
  - `crates/caco-web/src/bin/caco-web-observe.rs` — tool-icons subscenario + eval.
  - `crates/caco-web/src/tests.rs` — static guard for the glyph set.
- Tests: +1 live subscenario, +1 static guard.
- Behavioural delta: tool cards now show per-tool-type icons matching the TUI.

## Embedded artefacts

- None (bounded).

## Operator-takeaway

The render.rs-vs-web audit yielded a second parity fix (after notifications):
per-tool-type icons. Tool kinds are now visually distinguishable at a glance in
the web Pico pane, matching the TUI. Mock note: TranscriptItem::Tool requires a
ToolStatus enum (Running/Ok/Error) + a non-optional output string, not a free
"done" status -- the first mock attempt silently dropped the items.
