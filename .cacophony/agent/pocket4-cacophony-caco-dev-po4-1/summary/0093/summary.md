# Session summary — Stop CACO auto-generating AGENTS.md/CLAUDE.md in managed repos

## Goal

CACO-managed checkouts were auto-creating `AGENTS.md` (and symmetrically `CLAUDE.md`)
files that contained only a `# Managed Agent Constraints` block, in repositories
that never had those files (observed in the aurora-templates-te checkout). These
tracked project docs are unnecessary pollution. The goal was to find the
generation path, document why it existed, and make managed-constraint injection
append-only so it never creates a fresh tracked doc.

## Bead(s)

- `bd-0e5d84` — Investigate and remove AGENTS.md file generation in CACO repos

## Before state

- Failing tests: none
- `crates/caco-daemon/src/agent/spawn.rs`:
  - `inject_managed_codex_agents_md` created a new `AGENTS.md` (with just the
    managed block) when the checkout had none.
  - `inject_managed_claude_md` had the same create-if-missing behavior for
    `CLAUDE.md`.
- Both are called from three lifecycle sites (`lifecycle.rs:602`, `:1706`,
  `:9370`) during spawn/refresh for all agent types, so any managed checkout
  without these docs got fresh ones written.
- Test `inject_managed_claude_md_creates_new_file` asserted the create behavior;
  no Codex-side coverage existed.
- Note: the separate previous-summaries leak into these docs was already fixed
  earlier (bd-20d2dc / bd-eac6aa) by moving that context into prompt injection.

## After state

- Failing tests: none
- Both `inject_managed_claude_md` and `inject_managed_codex_agents_md` are now
  append-only: if the target doc does not already exist, they return early and
  write nothing. Existing docs are still appended-to idempotently.
- Managed constraints are still delivered to runtimes that lack these docs via
  the Pi `APPEND_SYSTEM.md` / `--append-system-prompt` path and the Claude
  checkout copy when present, so behavior for managed agents is preserved.
- Tests updated/added (all passing via queued `cargo test -p caco-daemon --lib`):
  - `inject_managed_claude_md_does_not_create_new_file` (replaces the old
    creates_new_file test)
  - `inject_managed_claude_md_appends_to_existing` (unchanged contract)
  - `inject_managed_claude_md_is_idempotent` (now seeds an existing file)
  - `inject_managed_codex_agents_md_does_not_create_new_file` (new)
  - `inject_managed_codex_agents_md_appends_to_existing` (new)
  - `inject_managed_codex_agents_md_is_idempotent` (new)
  - bd-20d2dc summary-startup tests re-run green (no regression).

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt
- Files touched:
  - `crates/caco-daemon/src/agent/spawn.rs` — append-only guard + doc comments
    in `inject_managed_claude_md` and `inject_managed_codex_agents_md`.
  - `crates/caco-daemon/src/agent/tests.rs` — rewrote Claude tests to the
    no-create contract and added a Codex test group.
- Tests: +3 (Codex group) / flipped 1 (claude creates_new_file -> does_not_create)
- Behavioural delta: managed spawn/refresh no longer writes `AGENTS.md` or
  `CLAUDE.md` into checkouts that lacked them; legitimate append-to-existing and
  idempotency are preserved.

## Operator-takeaway

CACO will no longer litter managed repos with one-block `AGENTS.md` / `CLAUDE.md`
files. The managed-constraints injection is strictly append-only now; if a repo
has no such doc, the constraints still reach the runtime through the
append-system-prompt path, so nothing about autonomous behavior changes — only
the unwanted tracked-file pollution is gone.
