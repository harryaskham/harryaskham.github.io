# bd-bf1e86 polish #15: chat empty state hints at compose keystroke

## Goal

Add the `Press 'i' to compose, Esc to dismiss.` hint to the chat view's empty state so a fresh chat doesn't leave the operator wondering how to start a conversation.

## Bead(s)

- bd-bf1e86 (permanent polish track) — cycle #15

## Before state

`crates/caco-tui/src/views/chat.rs` empty branch rendered exactly one DIM line `  No messages for this agent`. The compose keystroke hint already exists in the composer placeholder (`'i' to compose...`) but the empty messages region above it didn't reference it.

## After state

Empty messages region now renders three lines:
```
  No messages for this agent

  Press 'i' to compose, Esc to dismiss.
```

Both styled with `common::style_dim()` for visual cohesion with the existing message.

Verification:
- `cargo build -p caco-tui`: clean
- `cargo test-small`: 57/57 PASS
- `cargo clippy --workspace --all-targets -- -D warnings`: clean

## Diff summary

1 file changed, +10 / −4:

- `crates/caco-tui/src/views/chat.rs`: empty branch expanded to include hint line + bd-bf1e86 polish #15 comment

## Operator-takeaway

Last empty-state polish in the audit list. Coverage now: beads, notifications, crons, hooks, profiles, fuzzy_picker, chat. Builds/releases/tests already had domain-specific keystroke hints (`b`/`t`). Workspace_picker already had Esc/Enter hints. Empty-state-keystroke-hint parity is now project-wide.

bd-bf1e86 cycle counter: 15/session.
