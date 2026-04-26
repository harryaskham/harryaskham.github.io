# Session summary — terminal session broker design

## Goal

Design a Cacophony-owned terminal session broker so future embedded terminal renderers can consume local tmux, remote SSH/tmux, local shell, and AKS/private exec streams through one lifecycle and authorization contract.

## Bead(s)

- `bd-bf8064` — Design Cacophony terminal session broker for embedded panes

## Before state

- Failing tests: none.
- Relevant metrics: `bd-b0de8e` recommended a two-layer terminal architecture, but the broker layer did not yet have a concrete Cacophony design.
- Context: existing TUI/web/macOS terminal paths were spread across tmux, WebSocket, SSH, and helper-command assumptions.

## After state

- Failing tests: none in documentation validation.
- Relevant metrics: `docs/design/bd-bf8064-terminal-session-broker.md` now specifies session kinds, create/stream/resize/detach/terminate API shapes, WebSocket frame semantics, security, persistence, compatibility, and implementation acceptance checks.
- Context: `docs/logs.md` and README now point terminal renderer work at the broker design and preserve the split between live PTY streams and structured historical logs.

## Diff summary

- Commits: `bffe409ff`
- Files touched: `docs/design/bd-bf8064-terminal-session-broker.md`, `docs/logs.md`, `README.md`
- Tests: `docs/validate-pages.sh`; `git diff --check`
- Behavioural delta: no runtime code changed; this lands the design contract for future broker implementation slices.

## Operator-takeaway

Future macOS/Ghostty, web, and TUI embedded terminal work now has a shared broker target: renderers stay replaceable while Cacophony owns stream routing, auth, resize, detach, cleanup, and audit semantics.
