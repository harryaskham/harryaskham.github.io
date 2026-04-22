# Session summary 0016 — bd-aa4add slice 1: GETTING-STARTED.md

## Goal

Deliver the docs-side sister of the bd-aa4add 'caco bootstrap dev'
onboarding feature — a 5-minute walkthrough that takes a new operator
from zero through their first reintegrated agent. The CLI command
itself (scope items 2-5 of the parent bead) is filed as two
follow-ups.

## Bead(s)

- `bd-aa4add` — primary; this delivers the docs sister.
- Filed `bd-334962` (precondition checker `--check`) and `bd-ce32fa`
  (full interactive bootstrap) as follow-ups for the CLI work.

## Before state

- README.md had a brief Quickstart but no narrative onboarding doc.
- New operators had to piece together: build, config init, daemon
  start, project setup, agent spawn, attach, reintegrate — across
  multiple SPEC sections and trial-and-error.
- No troubleshooting table for the most common first-run failures.

## After state

- New top-level `GETTING-STARTED.md` (~170 lines) with:
  - Prerequisites (rust/cargo OR nix; tmux/sqlite/git).
  - 6 numbered steps from build through reintegrate.
  - "What just happened?" lifecycle summary tying the steps to the
    cluster's mental model.
  - "Next steps" pointing at SPEC sections for deeper learning.
  - Troubleshooting table covering the 4 highest-value first-run
    failures (port conflict, missing profile, agent runtime auth,
    post-restart stuck → bd-2b7a37).
- README.md Quickstart section gains a callout box pointing at
  GETTING-STARTED.md so the existing terse cheat-sheet stays for
  return visitors but new operators get the walkthrough.

## Diff summary

- Commit: `e8680309`.
- Files: `GETTING-STARTED.md` (new, +170), `README.md` (+5 callout).
- Tests: none added (pure docs).

## Out of scope (deferred)

- **`caco bootstrap dev --check`** precondition checker → bd-334962
  (P3, lower-risk first slice).
- **`caco bootstrap dev`** full interactive setup (config init,
  daemon start, sample project + bead, demo agent dispatch) →
  bd-ce32fa (P2, the heavier delivery).

## Operator-takeaway

A new operator can now read a single `GETTING-STARTED.md` file and end
up with a working daemon plus their first reintegrated agent without
flipping through SPEC.md or AGENTS.md. The CLI bootstrap-dev command
that automates these steps is filed for follow-up; until it lands,
GETTING-STARTED.md is the canonical onboarding path.
