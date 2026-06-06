# Session summary — Agent terminal full-pane layout

## Goal

Make the caco-web Agent Detail terminal more useful as a primary working surface by letting the terminal fill the available detail-pane height while preserving the existing fullscreen escape hatch.

## Bead(s)

- `bd-5b4ca1` — Implement full-height terminal layout for agents view

## Before state

- The caco-web Agent Detail terminal pane used a fixed `480px` height capped at `65vh`.
- The tab had a fullscreen button, but no intermediate full-pane mode that kept Agent Detail modal chrome and navigation available.
- Operators wanting a bigger terminal had to either use the fixed pane or jump to full viewport fullscreen.

## After state

- Agent Detail terminal tab body uses a flex layout so the PTY fills the available modal-body height.
- Added a full-pane toggle button (`▣`) in the terminal toolbar that expands the Agent Detail modal/body and terminal pane while keeping modal chrome visible.
- Existing fullscreen mode remains separate and unchanged.
- Leaving the Terminal tab or tearing down the terminal clears full-pane mode so other Agent Detail tabs render normally.

## Diff summary

- Code/content commits: `d82df2b96` (final landed squash SHA will come from the reintegration receipt).
- Files touched:
  - `crates/caco-web/static/app.js`
  - `crates/caco-web/static/style.css`
  - `crates/caco-web/src/tests.rs`
- Tests/validation:
  - `git diff --check` passed.
  - `node --check crates/caco-web/static/app.js` passed.
  - `cargo test -p caco-web agent_tty_full_pane_mode_is_wired_bd_5b4ca1 -- --test-threads=1` passed.
- Behavioural delta: Agent Detail terminal can now expand within the modal via full-pane mode and automatically refits/resizes the PTY after layout changes.

## Embedded artefacts

None.

## Operator-takeaway

The caco-web Agent Detail terminal now has an intermediate layout between cramped fixed pane and full-screen takeover: full-pane mode maximizes terminal height while keeping the Agent Detail context and modal controls available.
