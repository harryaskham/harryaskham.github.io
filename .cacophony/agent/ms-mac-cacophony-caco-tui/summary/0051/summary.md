# Session summary — Agent PTY stream kitty extraction

## Goal

Extend the kitty graphics passthrough work from TUI-owned shell PTYs into the agent-detail PTY stream abstraction so future daemon-delivered agent PTY bytes can surface plugin graphics instead of staying vt100-only.

## Bead(s)

- `bd-a5a372` — Agent PTY streams should pass through kitty graphics from plugins

## Before state

- Failing tests: none known for this path.
- Relevant metrics: not benchmarked; this is terminal byte-stream behavior.
- Context: `AgentPtyStream::feed` only pushed daemon-delivered bytes into a `vt100::Parser`; it had no stream-owned kitty extractor and no API for transport readers to recover raw or tmux-wrapped kitty payloads that may be split across daemon frames.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: not applicable.
- Context: `AgentPtyStream` now owns a `KittyGraphicsExtractor` and exposes `feed_with_kitty_graphics`, which buffers split raw APC and tmux passthrough payloads across frames while still updating the vt100 parser. Existing `feed` remains as a compatibility wrapper.

## Diff summary

- Commits: `51057fd38`
- Files touched: `crates/caco-tui/src/agent_pty_stream.rs`
- Tests: +3 focused agent PTY stream kitty extraction tests
- Behavioural delta: daemon-stream transports have a first-class API to extract complete kitty graphics payloads from agent PTY output, including split-frame payloads, before forwarding them to the outer graphics-capable terminal.
- Validation:
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui feed_with_kitty_graphics --lib`

## Operator-takeaway

The owned-shell PTY passthrough path now has matching support in the agent PTY stream abstraction: future daemon stream readers can preserve plugin kitty graphics without re-solving split-frame parsing.
