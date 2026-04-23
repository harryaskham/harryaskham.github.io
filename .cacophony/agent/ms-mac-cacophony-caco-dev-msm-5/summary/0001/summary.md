# Session summary — axe-core a11y gate (bd-a2ace0)

## Goal

Close the rediscover-every-cycle accessibility gap noted by the
workspace-view polish/a11y permanent (bd-09f314). Each cycle was
manually re-running axe devtools or wave and surfacing the same
categories of defect (missing aria-label, missing role, low contrast).
Automate it: render the workspace panes into a real DOM and run
axe-core programmatically, failing the build on any serious/critical
violation.

## Bead(s)

- `bd-a2ace0` — workspace-view: ship an in-repo axe-core test pass over
  rendered pane HTML
- parent permanent: `bd-09f314` — workspace-view polish + a11y

## Before state

- Failing tests: none in caco-web
- a11y was enforced by manual cycles of bd-09f314, with no machine-
  checkable contract. Drive-by refactors could quietly regress
  WCAG-level invariants without detection until the next cycle.

## After state

- Failing tests: none. `cargo test -p caco-web --lib` = 102 passed
  (+3 new). `cargo clippy -p caco-web --tests` clean.
- With jsdom installed (`NODE_PATH=.../jsdom cargo test -p caco-web
  --lib axe`) the harness runs the real axe pass and reports 0 / 0
  violations on the rendered bead-detail pane. Self-test confirms the
  harness still detects injected defects.
- Without jsdom, the test soft-skips with an actionable install hint —
  no false negative, no false positive.

## Diff summary

- Commits: see `bd-a2ace0` footer
- Files touched:
  - `crates/caco-web/static/vendor/axe/axe.min.js` (vendored upstream
    4.11.3, MPL-2.0)
  - `crates/caco-web/static/vendor/axe/README.md`
  - `crates/caco-web/tests/axe_harness.js` (jsdom + axe pipeline)
  - `crates/caco-web/tests/axe_self_test.js` (reverse-pin)
  - `crates/caco-web/tests/.gitignore` (do not commit node_modules)
  - `crates/caco-web/src/tests.rs` (+3 tests)
- Tests: +3 / -0 / flipped 0
- Behavioural delta: none for production; new test gate enforced
  whenever jsdom is resolvable.

## Operator-takeaway

The a11y enforcement is now a contract, not a permanent-cycle ritual.
The reverse-pin self-test is the important second piece — it means a
future refactor that breaks the harness itself will fail loudly rather
than silently passing every pane. To turn the soft-skip into a hard
gate locally or in CI, install jsdom: `npm install --no-save jsdom@26`
in the workspace root or set NODE_PATH to a directory containing it.
