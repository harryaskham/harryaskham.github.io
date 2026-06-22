# Session summary — bd-238cea: style.css .tab-btn + .tab-btn.active paired-selector dedup (bd-f0393a sibling)

## Goal

Continue the bd-f0393a / bd-20aee5 / bd-b2b739 /
bd-3dfff5 / bd-39161c / bd-c4e2ee / bd-0cb4d9 /
bd-f479b0 dead-rule / dup-block audit. The tab-
button selector family had TWO duplicate-selector
pairs — `.tab-btn` ×2 and `.tab-btn.active` ×2 —
both with silent-override patterns.

## Bead(s)

- `bd-238cea` — [caco-web] style.css .tab-btn + .tab-btn.active paired-selector dedup

## The four blocks

### `.tab-btn` pair
- Line 3361 canonical: padding/font/etc + `transition: all var(--transition)`.
- Line 7347 late refinement: `transition: background-color 150ms ease, color 150ms ease, box-shadow 150ms ease` only.

Cascade: `all var(--transition)` silently overridden. Visible truth = specific property-list transition.

### `.tab-btn.active` pair
- Line 3380 canonical: `color: var(--accent)`, `border-bottom-color: var(--accent)`.
- Line 7343 late refinement: `box-shadow: inset 0 -2px 0 var(--accent), 0 0 12px rgba(...)`, `color: var(--text, var(--nord6))`.

Cascade: `color: var(--accent)` silently overridden; `border-bottom-color` and `box-shadow` both survive.

## Fix (paired-selector atomic merge)

Two-pair atomic dedup preserving current visible behavior:

1. **`.tab-btn`** canonical at 3361: promoted specific-property-list transition. Late dup at 7347 deleted.
2. **`.tab-btn.active`** canonical at 3380: promoted `color: var(--text, var(--nord6))` + `border-bottom-color: var(--accent)` survives + promoted inset/glow `box-shadow`. Late dup at 7343 deleted.

Two consolidated marker comments document the merges.

## Test design (8 layers)

1. Exactly 1 `.tab-btn {` rule head (was 2).
2. Exactly 1 `.tab-btn.active {` rule head (was 2).
3. Merged `.tab-btn` block preserves structural baseline + promoted transition.
4. NEGATIVE: dead `transition: all var(--transition);` must remain removed (bounded char-window).
5. Merged `.tab-btn.active` block contains promoted color + surviving border-bottom-color + promoted box-shadow.
6. **NEGATIVE: dead standalone `color: var(--accent);` must remain removed from `.tab-btn.active` block — anchored to leading-whitespace `\n    color: var(--accent);` to avoid false-positive substring tail of `border-bottom-color: var(--accent);` which is the SURVIVING rule.**
7. Two replacement marker comments document each merge.
8. bd-f0393a sibling pattern presence pin.

## Lesson learned

**NEGATIVE-assertion anchor pattern**: initial
assertion used unanchored substring
`color: var(--accent);` which matched as a SUBSTRING
TAIL of the surviving `border-bottom-color:
var(--accent);` declaration. Fixed by anchoring to
leading indent `\n    color: var(--accent);` which
disambiguates the standalone color property from the
longer border-bottom-color property.

Per critical context's "beware substring collisions
in marker comment text vs assertion strings"
pattern, extended to **property-name substring
collisions** where one declaration name is a tail of
another (color vs border-bottom-color). Future
NEGATIVE assertions for CSS property substrings must
anchor to leading whitespace or use word-boundary
markers.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted to avoid self-referential SHA.
- Files touched:
  - `crates/caco-web/static/style.css` -- 2 canonical blocks merged with promoted truths; 2 late dup blocks deleted; 4 marker comments added.
  - `crates/caco-web/src/tests.rs` -- new bd-238cea regression test with 8 assertion layers including TWO bounded-char-window NEGATIVE assertions with leading-whitespace anchoring for property-name substring disambiguation.
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*` -- bounded validation receipts.
- Tests: +1 caco-web static asset regression test. Net pass count: 518 -> 519; 0 failures.

## Operator-takeaway

style.css continues shrinking. The bd-f479b0 ->
bd-0cb4d9 -> bd-c4e2ee -> bd-39161c -> bd-3dfff5 ->
bd-b2b739 -> bd-20aee5 -> bd-f0393a -> bd-238cea
family chain now demonstrates dedup patterns for:
(a) shared late-shadowing additions, (b) pure byte-
identical duplicates, (c) compound-vs-standalone
selector disambiguation, (d) silent-override
NEGATIVE assertion, (e) visual-composition
preservation, (f) orphan-`@keyframes` cleanup, (g)
NEGATIVE-assertion marker-text conflict resolution,
(h) strictly-additive consolidation, **(i) paired-
selector atomic merge — two related selectors
deduped together in one cycle so the test can guard
visible composition cross-selector, (j) NEGATIVE-
assertion leading-whitespace anchoring for property-
name substring disambiguation (color vs
border-bottom-color)**.
