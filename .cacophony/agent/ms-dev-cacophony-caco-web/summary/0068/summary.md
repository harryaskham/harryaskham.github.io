# Session summary — bd-327d96: runtime-input form-hygiene attrs

## Goal

A prior a11y/UX pass added `autocomplete="off"`,
`autocorrect="off"`, `autocapitalize="off"`,
`spellcheck="false"` to all static `index.html` search
inputs. Runtime-generated inputs from JS templates were
missed. Bring them in line so browser autofill, spellcheck
red-underlines, and mobile auto-capitalisation no longer
clutter technical-identifier and search/filter inputs.

## Bead(s)

- `bd-327d96` — [caco-web] autocomplete/spellcheck/autocorrect/autocapitalize=off on 10 runtime-generated inputs

## 10 inputs covered

| File | Element |
|---|---|
| `app.js` | TTS daemon-model `<input type="text">` |
| `app.js` | TTS daemon-speed `<input type="number">` |
| `app.js` | `#agent-tty-search` |
| `app.js` | `#agent-nudge-input` |
| `app.js` | `#agent-logs-search` |
| `summaries.js` | `#summaries-project` |
| `summaries.js` | `#summaries-agent` |
| `summaries.js` | `#summaries-bead` |
| `workspace-bead-list-pane.js` | `.wbl-search` |
| `workspace-keyboard.js` | `.wsv-palette__input` |

## Attribute subset per input

| Kind | Attrs |
|---|---|
| text / search (technical IDs, search) | `autocomplete="off" autocorrect="off" autocapitalize="off" spellcheck="false"` |
| numeric input | `autocomplete="off"` (others don't apply) |
| chat-style text (agent-nudge) | `autocomplete="off"` (preserve OS autocorrect / spellcheck so messages benefit) |

The agent-nudge case is deliberately less aggressive:
operators want OS spellcheck/autocorrect for actual prose
messages, but no stored-form-data autofill suggestions.

## UX improvements

- No "your stored form data" dropdown when clicking bead
  search, agent-logs filter, or workspace command palette.
- No red spellcheck underlines when typing `bd-1234`,
  `tts-1-hd`, or agent IDs.
- No mobile auto-capitalisation of first character on
  technical identifiers.
- No Safari autocorrect munging `tts` / abbreviation
  values.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted to avoid self-referential SHA.
- Files touched:
  - `crates/caco-web/static/app.js` -- 5 inputs (TTS model/speed, agent-tty-search, agent-nudge-input, agent-logs-search).
  - `crates/caco-web/static/summaries.js` -- 3 inputs (summaries-project/agent/bead).
  - `crates/caco-web/static/workspace-bead-list-pane.js` -- .wbl-search.
  - `crates/caco-web/static/workspace-keyboard.js` -- .wsv-palette__input.
  - `crates/caco-web/src/tests.rs` -- regression test pins each of the 10 attribute additions individually + a count-based assertion for the summaries filter quartet.
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*` -- bounded validation receipts.
- Tests: +1 caco-web static asset regression test. Net pass count: 474 -> 475; 11 pre-existing failures on main unchanged.

## Operator-takeaway

The 10 runtime-generated technical-identifier and search
inputs now behave like the static-HTML search inputs:
clean, no browser-autofill noise, no spellcheck red, and
no mobile capitalisation interference. Combined with the
prior 18 perf/polish wins this session, the dashboard's
text input UX is finally uniform.
