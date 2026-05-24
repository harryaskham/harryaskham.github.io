# Session summary — Visiting agent registration

## Goal

Implement the first slice of dynamic/visiting agent support by adding a discoverable `caco agent register` command that an unmanaged shell can use to register itself as a visible Cacophony visitor without materializing or rewriting a managed checkout.

## Bead(s)

- `bd-14a795` — Add caco agent register for dynamic visiting agents

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: no existing `caco agent register` command or `caco_agent_register` MCP metadata entry.
- Context: visiting-agent behavior was not documented in `SPEC.md`; the implementation had only managed-agent creation paths that launch tmux/checkouts.

## After state

- Failing tests: none observed in targeted validation.
- Relevant metrics: `cargo check -p caco-cli` passed; targeted `caco-cli` unit tests for registration metadata and dynamic project-name derivation passed.
- Context: the command metadata, MCP metadata, CLI dispatcher, agent metadata types, persistence path, and SPEC contract now cover the first visiting-agent registration slice.

## Diff summary

- Code/content commits: `9ce9311e1`
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `SPEC.md`, `crates/caco-cli/src/lib.rs`, `crates/caco-daemon/src/agent/lifecycle.rs`, `crates/caco-daemon/src/agent/types.rs`
- Tests: +2 targeted unit tests / -0 / flipped 0
- Behavioural delta: `caco agent register` is exposed in CLI/MCP metadata, rejects managed Cacophony environments, derives a deterministic dynamic project from the current directory, and persists a visiting-agent inventory record without creating a checkout.

## Operator-takeaway

The first registration slice is intentionally lightweight: it makes unmanaged visitors visible through the normal agent inventory metadata without touching their working tree. Follow-up work can build richer routing/attach semantics on that persisted visitor record.
