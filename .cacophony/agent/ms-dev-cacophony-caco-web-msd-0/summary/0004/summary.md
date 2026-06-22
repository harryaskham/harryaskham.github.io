# Session summary — bd-421072 slice 2a: delegated handlers for controlled-ID onclicks

## Goal

Continue the caco-web inline-onclick-with-user-data security sweep (bd-66018c /
bd-421072). Slice 1 already fixed the genuine free-text vector (copyToClipboard)
via a `data-copy-text` + delegated handler. This slice converts a batch of the
remaining controlled-ID inline-onclick sites to the same safe data-* +
delegated-handler pattern, for defense-in-depth consistency.

## Bead(s)

- `bd-421072` — caco-web: sweep dashboard inline-onclick-with-user-data for the
  bd-66018c single-quote breakout class. Stays `in_progress` (footer mutation
  buttons + rows remain for later slices).
- Related: `bd-66018c` (the original Pico-pane fix that established the pattern).

## Before state

- Failing tests: none.
- 8 isolated single-handler sites used `onclick="fn('${escapeAttr(x)}')"`. That
  is unsafe in principle: the HTML parser decodes `&#39;` back to `'` in the
  attribute value before the onclick JS runs, so escapeAttr does not prevent a
  breakout. (In practice these pass controlled values — agent ids, bd-XXXXXX,
  fixed filter enums — that cannot contain a single quote, so the practical risk
  is near-zero; this is defense-in-depth.)
- The delegated document click handler only handled `data-copy-text`.

## After state

- Failing tests: none (node --check clean; no tests.rs assertions reference the
  converted sites; the accessibility test at tests.rs:5358 only inspects
  tabindex=0 elements, which these are not).
- 8 sites now carry data-* attributes; the delegated handler (app.js ~12488)
  dispatches them: data-show-agent-detail, data-show-bead-detail,
  data-agent-state-filter, data-bead-status-filter. The copy branch now returns
  so a nested copy button does not also navigate.
- Live-dashboard validated (dev server serving this checkout's static): beads
  status filter chip and agents state filter chip both activate via the
  delegated handler; agent-card click navigates to #agent/<id>; console clean.

## Diff summary

- Code commit: 1db3bf5e5a (this checkout). Final landed squash SHA from the
  reintegration receipt.
- Files touched: crates/caco-web/static/app.js only.
- Sites converted (8): agent summary cards (x2), chat sender + avatar,
  pinned-bead chip, bead-close ref link, agent-state filter chip, bead-status
  filter chip.
- Behavioural delta: none intended — clicks navigate/filter exactly as before,
  now via a non-eval data-* path. Tests: +0 / -0.

## Embedded artefacts

- screenshots/slice2a-agent-nav.png — dashboard after an agent-card click
  navigated to the agent detail (#agent/<id>), captured via headless chromium
  against the dev server serving this checkout's static.

## Operator-takeaway

The genuine onclick-injection risk in caco-web (free-text copyToClipboard) was
already fixed and landed in slice 1. The remaining bd-421072 sites pass
controlled values (ids / fixed enums) that cannot carry the breakout character,
so this and subsequent slices are defense-in-depth hardening, intentionally done
in small validated batches rather than one risky core-navigation refactor.
Deferred: bead-detail footer mutation buttons (claim/dispatch/unclaim/move/pin,
which carry a `this` loading-state arg) and the agent/bead table rows (which also
have inline onkeydown handlers coupled to tests.rs:5080-5096).
