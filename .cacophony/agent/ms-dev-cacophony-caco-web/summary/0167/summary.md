# Session summary — bd-9b73db: createElement button type defense

## Goal
Pattern (m) button type=button defense for createElement-built buttons: 12 createElement('button') calls had default 'submit' type behaviour.

## Bead
- `bd-9b73db`

## Audit
- All createElement('button') scanned with next-8-line .type assignment check.
- 12 candidates: 8 workspace-integrated.js + 4 workspace-terminal-pane.js.

## Fix
- Each createElement('button') gains following `X.type = 'button';`.

## Why
- Default `<button>` type is 'submit' per HTML spec.
- Defense-in-depth against future form-wrapping regressions.

## Regression test (~35 lines)
- Per file: type-count >= create-count.

## Operator-visible effect
- None today; prevents future submit-on-click regression.

## Diff summary
- `crates/caco-web/static/workspace-integrated.js` -- 8 .type=button additions.
- `crates/caco-web/static/workspace-terminal-pane.js` -- 4 .type=button additions.
- `crates/caco-web/src/tests.rs` -- new bd-9b73db forward-guard (~35 lines).
- Net pass: 583 -> 584; 0 failures.

## Operator-takeaway
75 cycles, 118 wins. Pattern (m) button type defense. Pattern catalog: 22 entries.
