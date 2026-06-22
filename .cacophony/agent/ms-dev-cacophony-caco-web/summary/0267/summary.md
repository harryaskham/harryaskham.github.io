# Session summary — bd-64251b: web footer context_max parity (ctx 42%/200k)

## Goal

Close a native conversation-display parity gap found by comparing the web footer
against the shared caco-picophony render: the shared render (caco-tui/native)
shows the context indicator as "{pct}%/{humanized context_max}" (e.g. 42%/200k),
but the web footer showed only "ctx {pct}%" and had zero references to
context_max — so web operators never saw the absolute context window size.

## Bead(s)

- `bd-64251b` — footer omits context_max (native shows ctx 42%/200k, web shows only 42%)

## Before state

- Failing tests: none. renderPicoFooter rendered `ctx ${pct}%` only; context_max
  (a real snapshot field carried through the wasm) was unused by the web.

## After state

- Failing tests: none. Added picoHumanizeTokens(n) mirroring caco-picophony
  humanize_tokens (>=1e6 -> "Xm"/"X.Xm" @0.05 fract; >=1e3 -> floor/1000 + "k";
  else String(n)). renderPicoFooter now shows "ctx {pct}%/{humanized max}" when
  context_max is known, falling back to "ctx {pct}%" otherwise. New live
  subscenario asserts "ctx 64%/200k" from context_percent 64 + context_max
  200000; the existing footer-indicators test (null max) still shows "ctx 42%".
  2/2 clean.
- caco-web bin 12; `--lib` 651; clippy clean.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-web/static/app.js` — picoHumanizeTokens + footer context_max parity.
  - `crates/caco-web/src/bin/caco-web-observe.rs` — context_max mock + subscenario + eval.
- Tests: +1 live subscenario.
- Behavioural delta: the web footer now shows the absolute context window like every native surface.

## Embedded artefacts

- None.

## Operator-takeaway

Found by a systematic web-vs-shared-render field comparison (the same method that
found the send-state gap): context_max was the one shared-render footer field the
web ignored. The web context indicator now matches caco-tui/macOS/iOS/Android.
