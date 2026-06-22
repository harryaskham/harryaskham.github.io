# Session summary — bd-39653b: inputmode=decimal for TTS speed

## Goal
Pattern (m) inputmode mobile-keyboard hint: 1 numeric input with fractional step missing inputmode.

## Bead
- `bd-39653b`

## Audit
- All `<input>` of type number/tel/email/url scanned.
- Yield: 1 candidate (app.js:7606 TTS daemon speed).

## Fix
- Added `inputmode="decimal"` to TTS speed input (`type="number" min="0.25" max="4" step="0.05"`).

## Why
- step="0.05" → fractional values (1.05, 1.5, 2.25 etc).
- iOS Safari without inputmode="decimal" shows integer keypad with decimal point hidden behind punctuation toggle.

## Regression test (~50 lines)
- Locates speed input via `setDaemonSpeed` onchange.
- Walks back to opening `<input` tag.
- Asserts `inputmode="decimal"` present.
- Asserts type/min/max/step preserved.

## Operator-visible effect
- iOS / Android users get decimal-friendly keypad.
- No desktop change.

## Diff summary
- `crates/caco-web/static/app.js` -- 1 attribute added.
- `crates/caco-web/src/tests.rs` -- new bd-39653b forward-guard (~50 lines).
- Net pass: 566 -> 567; 0 failures.

## Operator-takeaway
57 cycles, 100 wins! Pattern (m) inputmode hint. Pattern catalog: 22 entries.
