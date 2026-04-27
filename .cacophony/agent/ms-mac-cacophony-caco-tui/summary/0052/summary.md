# Session summary — Normalize PTY kitty passthrough payloads

## Goal

Fix a follow-up edge case from the TUI-owned PTY kitty passthrough work: tmux passthrough wrappers should only be sent to an outer tmux, while a direct kitty-capable terminal needs the raw kitty APC payload.

## Bead(s)

- `bd-4085b8` — TUI kitty passthrough should unwrap tmux payloads outside tmux

## Before state

- Failing tests: none; focused normalization coverage did not exist.
- Relevant metrics: not benchmarked; this is byte-level terminal compatibility behavior.
- Context: the shell PTY passthrough event forwarded tmux DCS passthrough payloads unchanged. That works if the outer TUI is itself inside tmux, but a direct kitty-capable terminal can ignore the tmux wrapper.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: not applicable.
- Context: outgoing kitty passthrough payloads are normalized: raw APC stays raw outside tmux, raw APC is wrapped inside tmux, tmux-wrapped payloads are unwrapped outside tmux, and tmux-wrapped payloads stay wrapped inside tmux.

## Diff summary

- Commits: `0ede6d796`
- Files touched: `crates/caco-tui/src/kitty.rs`, `crates/caco-tui/src/app.rs`
- Tests: +4 focused payload-normalization tests
- Behavioural delta: nested tmux-produced kitty graphics now have the correct outer-terminal transport whether the TUI runs directly in a kitty-capable terminal or inside another tmux.
- Validation:
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui normalize_kitty_passthrough_payload --lib`

## Operator-takeaway

The passthrough path now distinguishes direct terminals from tmux-hosted terminals, avoiding the case where a non-tmux kitty terminal receives and ignores an unnecessary tmux DCS wrapper.
