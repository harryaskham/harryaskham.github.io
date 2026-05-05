# Session summary — TUI upload scheduling docs

## Goal

Run the technical-writer review pass, audit recent commits, and update GitHub Pages where implementation-visible TUI graphics scheduling behaviour had drifted from the docs.

## Bead(s)

- `bd-df49a9` — Split pure TUI graphics backoff ticking from upload scans
- `bd-6cd101` — Defer tmux pane-origin refresh until Kitty placement uploads

## Before state

- Failing tests: none known.
- Relevant metrics: previous Pages validation was clean.
- Context: Recent TUI commits changed the upload scheduling path so pure retry-backoff ticks avoid full upload scans and tmux pane origin refresh only happens when a pass is about to emit placement commands. The existing Pages text still described upload/backoff together without that distinction.

## After state

- Failing tests: none observed.
- Relevant metrics: `./docs/validate-pages.sh` passed with `3313 passed, 0 warnings, 0 failed`.
- Context: `docs/tui.html` now documents the split between fetch/upload/retry-backoff/native-animation work and the cheap pure-backoff path, plus the placement-only tmux origin refresh.

## Diff summary

- Commits: `70e6713ec`
- Files touched: `docs/tui.html`
- Tests: documentation validation only; `./docs/validate-pages.sh` passed.
- Behavioural delta: no runtime behavior changed; Pages now matches the current TUI graphics upload scheduler.

## Operator-takeaway

TUI graphics scheduling evidence should be read with the new distinction: retry-backoff can tick cheaply without a full upload scan, and tmux pane-origin refresh is paid only by passes that actually write Kitty placement commands.
