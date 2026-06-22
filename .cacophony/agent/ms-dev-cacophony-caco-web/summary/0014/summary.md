# Session summary — bd-0e0149: technical filter inputs stop mobile autocorrect mangling identifiers

## Goal

Continue the caco-web frontend perf/visual/UX polish loop with a targeted
mobile UX fix: stop the soft keyboard from mutating technical identifier
text in the dashboard's filter inputs.

## Bead(s)

- `bd-0e0149` — [caco-web] technical search/filter inputs autocorrect identifiers on mobile (UX)

## Before state

Eight filter inputs hold technical identifiers (bead IDs like `bd-55c6bf`,
agent IDs, project slugs, file paths, workspace slugs):
`#agent-search`, `#artefacts-filter-workspace`, `#files-search`,
`#links-search`, `#bead-search`, `#feed-filter`, `#logs-filter`,
`#tui-terminal-agent-id`. None declared `autocomplete`, `autocorrect`,
`autocapitalize`, or `spellcheck`, so iOS/Android keyboards would
autocorrect `bd-` to `BD-`, capitalise the first character, suggest
history entries that occlude live filter results, and underline valid
identifiers as misspellings.

## After state

- All 8 technical-identifier filter inputs declare
  `autocomplete="off" autocorrect="off" autocapitalize="off"
  spellcheck="false"`.
- Natural-language inputs (chat textarea, bead title/description, bead
  labels, quick-bead textarea) intentionally stay on default behaviour
  so normal English still autocompletes and autocorrects.
- New `technical_filter_inputs_disable_mobile_autocorrect_bd_0e0149`
  test enumerates the 8 IDs and asserts each one carries every required
  attribute, locking in the contract for future filter inputs.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted to avoid self-referential SHA.
- Files touched:
  - `crates/caco-web/static/index.html` — added 4 attributes to 8 filter inputs.
  - `crates/caco-web/src/tests.rs` — added regression test.
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*` — bounded validation receipts.
- Tests: +1 caco-web static asset regression test.

## Operator-takeaway

Typing a bead ID like `bd-0e0149` into the bead search on iOS no longer
becomes `Bd-0E0149` with red squiggles and a history dropdown blocking
the result list. Same fix applies to agent search, files/links search,
feed/logs filters, and the TUI terminal agent-id input.
