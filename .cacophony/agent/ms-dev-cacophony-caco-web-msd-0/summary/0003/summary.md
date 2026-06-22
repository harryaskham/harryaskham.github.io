# Session summary — caco-web copyToClipboard inline-onclick security slice (bd-421072)

## Goal

Close the genuine free-text injection vector in the dashboard-chrome
inline-onclick-with-user-data security class (bd-421072, filed by sibling
ms-dev-cacophony-caco-web after fixing the Pico instance in bd-66018c). Convert
the copyToClipboard inline onclick call sites — the sites whose interpolated
value can be free text (copied URLs) — to a safe data-* + delegated-handler
pattern.

## Bead(s)

- `bd-421072` — caco-web: sweep dashboard inline-onclick-with-user-data for the
  bd-66018c single-quote breakout class (claimed; copyToClipboard slice
  implemented this cycle; controlled-ID sites remain as follow-up).

## Before state

- `onclick="copyToClipboard('${escapeAttr(x)}')"` at 7 call sites (beadId,
  bead.id, file.id, link.url, link.id, agent.id x2). escapeAttr turns `'` into
  `&#39;`, but the HTML parser decodes `&#39;` back to `'` in the attribute value
  BEFORE the onclick JS runs — so a quote breaks the handler and free text
  (e.g. a copied URL) can inject JS (escapeAttr does not escape `()`/commas).

## After state

- A delegated `[data-copy-text]` click handler is added to the existing
  document-level click listener; the 7 sites now use
  `data-copy-text="${escapeAttr(x)}"`. dataset reads are a plain-string
  (non-eval) context, so `&#39;` decodes to a literal `'`.
- Validated live: a button with `data-copy-text="id');alert(1);//(a,b)"`
  round-trips the exact string through `dataset.copyText` and reaches
  `copyToClipboard` verbatim — no breakage, no JS evaluation (SAFE_PASS).
- `node --check` OK; the safe arrow `copyToClipboard(err)` refs (asserted by the
  `Copy error` JS test) are untouched; `git diff --check` clean.

## Diff summary

- Code commits: pending final squash SHA from the reintegration receipt.
- Files touched: `crates/caco-web/static/app.js` (+delegated handler; 7 onclick
  -> data-copy-text conversions). +13 / -6 lines.
- Behavioural delta: copy buttons keep working via delegation; free-text copy
  values (URLs) can no longer break the handler or inject JS.

## Operator-takeaway

`onclick="fn('${escapeAttr(x)}')"` is NOT safe for any value that can contain a
quote/parens/comma, because attribute-value HTML-decoding happens before the
inline JS is evaluated. The fix is the bd-66018c pattern: data-* attribute +
delegated handler reading dataset. This slice closes the free-text copyToClipboard
vector; the remaining bd-421072 sites (showAgentDetail, showBeadDetail, claimBead,
setBeadStatusFilter, setAgentStateFilter, togglePinBead) pass controlled IDs
(lower practical risk) and are a documented follow-up slice.
