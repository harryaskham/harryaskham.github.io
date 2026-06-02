# Session summary — caco-web: de-duplicate agent-detail Complete button

## Goal

First duty cycle for the persistent caco-web agent on a fresh microVM node.
No ready caco-web beads existed and this node has no browser (NixOS, no
chromium / cached Playwright browser), so a live Playwright observation pass
was not feasible. Instead I ran a source-level audit of the embedded
dashboard assets for the bd-fc3328 "advertised control / duplicate control"
defect class, found a real one, and fixed it with a regression test.

## Bead(s)

- `bd-3fe2f8` — caco-web: agent detail modal renders two identical Complete buttons (filed + claimed + fixed this cycle)
- corroborated existing draft `bd-9bd6ac` — `caco bd create` shell-substitution-damage guard false-positives on `--description-file`/`--stdin` (addendum via msg send, not a duplicate filing)

## Before state

- Failing tests: none known.
- Defect: in `crates/caco-web/static/app.js` the live-agent Agent Detail modal
  action row rendered TWO adjacent identical `data-agent-action="complete"`
  buttons (one `btn-success`, one plain), both gated on the same `liveControls`
  condition. Confirmed via git history: commit `74d49a4ba` rewrote a former
  Reintegrate button into a Complete button but left the pre-existing plain
  Complete button in place.
- `data-agent-action="complete"` occurrences in app.js: 2.

## After state

- Failing tests: none. New regression test
  `agent_detail_complete_button_not_duplicated_bd_3fe2f8` passes (queued job
  `tj-60fe4581`, exit 0).
- `data-agent-action="complete"` occurrences in app.js: 1. The live-agent
  action row is now stop, pause, nudge, restart, fork, complete, resume,
  discard — no duplicate control.
- `node --check crates/caco-web/static/app.js`: syntax OK.

## Diff summary

- Code/content commit: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-web/static/app.js` — removed the redundant plain `btn-sm`
    Complete button, keeping the single `btn-success` primary Complete button (-1 line).
  - `crates/caco-web/src/tests.rs` — added regression test
    `agent_detail_complete_button_not_duplicated_bd_3fe2f8` asserting exactly
    one Complete button.
- Tests: +1 / -0 / flipped 0.
- Behavioural delta: the Agent Detail modal for a live agent now shows a single
  Complete button instead of two identical ones; `Shift+C` behaviour unchanged
  (it already only clicked the first match).

## Embedded artefacts

- None. No browser available on this microVM node, so no Playwright
  screenshots/console/network for this cycle; the defect and fix are fully
  source-verifiable and locked by a unit test.

## Operator-takeaway

A live-agent control row had been quietly showing two identical "Complete"
buttons since commit `74d49a4ba` (Reintegrate→Complete rewrite that left the
old Complete in place); now de-duplicated and guarded by a regression test.
Note: this caco-web node is a browserless microVM, so future cycles here will
be source-audit + queued-test driven rather than Playwright-driven unless a
nix-provided chromium is made available (or the agent is relocated to a host
that can run the dashboard).
