# Session summary — libghostty embedded terminal feasibility

## Goal

Investigate whether Cacophony should use Ghostty/libghostty for native embedded terminal panes in the macOS app rather than implementing terminal emulation from scratch.

## Bead(s)

- `bd-b0de8e` — [macOS terminal] Investigate libghostty for embedded terminal panes
- Follow-ups filed as drafts: `bd-dd5935`, `bd-bf8064`, `bd-efccec`

## Before state

- Failing tests: none; this was a research/documentation slice.
- Relevant metrics: current Cacophony terminal control is centered on tmux, SSH, raw attach, and TUI/web surfaces; the macOS app does not yet own a full embedded terminal view.
- Context: the operator wanted a feasibility assessment for excellent embedded local/remote terminal panes with graphics support.

## After state

- Failing tests: none.
- Relevant metrics: research doc now covers libghostty API shape, MIT licensing, Swift/macOS integration options, host-managed I/O fit, Kitty graphics support, risks, and recommended prototype architecture.
- Context: recommendation is to prototype behind a Cacophony-owned terminal session broker before adding a production dependency.

## Diff summary

- Commits: `eae638bce`
- Files touched: `docs/investigations/bd-b0de8e-libghostty-embedded-terminals.md`
- Tests: `git diff --check`
- Behavioural delta: no runtime code changed. The output is a feasibility assessment plus concrete follow-up beads.

## Operator-takeaway

libghostty looks promising for Cacophony terminal panes, especially because it supports host-managed I/O and modern graphics, but its API and Swift wrapper ecosystem should be isolated behind a Cacophony terminal broker and proven in a prototype before production adoption.
