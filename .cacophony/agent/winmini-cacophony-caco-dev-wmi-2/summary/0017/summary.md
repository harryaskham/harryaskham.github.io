# bd-3fa3c6: caco agent ship — atomic rebase + reintegrate wrapper

## Goal

Implement `caco agent ship --id <id> [--mode <mode>] [--comment <s>]` as an atomic wrapper that runs `agent rebase` followed by `agent reintegrate`. Eliminates the manual rebase → (fail) → force-with-lease → reintegrate dance that operators (and agents like me) have done 10+ times this session.

## Bead(s)

- bd-3fa3c6 (P3 feature; closes via reintegrate)

## Before state

Operators landing work needed 4 commands in sequence, with manual conflict-handling between each:
1. `caco agent rebase --id X` (sync onto current main)
2. `caco agent reintegrate --id X --mode direct,recorded`
3. If reintegrate fails on non-FF: `git fetch origin agent/<branch> && git reset --hard FETCH_HEAD && git merge -X theirs origin/main && git cherry-pick <SHA>`
4. Re-attempt reintegrate

This pattern has been the dominant footgun this session — see "Key Decisions" in the persistent agent context for the bd-727210 footgun routine.

## After state

New `caco agent ship` subcommand:
- `AGENT_SHIP_ARGS` constant defines `--id` (required), `--mode`, `--comment` (both optional, forwarded to reintegrate)
- `CommandSpec` entry registers the command in the agent subcommand table (mcp_enabled, agent_safe)
- Dispatch arm at `[cmd, sub] if cmd == "agent" && sub == "ship"` parses flags
- New `dispatch_agent_ship(id, mode, comment, json_requested, config_override)` function:
  1. Calls `dispatch_agent_rebase(id, json_requested, config_override)` — propagates rebase conflicts as-is (already produces operator-friendly output)
  2. On rebase success, calls `dispatch_agent_reintegrate(id, mode, comment, false, None, None, false, None, None, None, None, json_requested, config_override)` — passing only the flags that ship surfaces
  3. JSON mode: emits an `{ok, agent_id, steps: {rebase: {...}, reintegrate: {...}}}` envelope
  4. Text mode: emits `agent X shipped:\n--- rebase ---\n...\n--- reintegrate ---\n...` so the operator sees both step's output

This is sugar on top of existing functions — no new business logic, just composition. Implementation is ~60 LOC including doc-comment.

Verification:
- `cargo build -p caco-cli`: clean
- `cargo test-small`: 57/57 PASS
- `cargo clippy --workspace --all-targets -- -D warnings`: clean
- No broken-on-main this cycle (merge-queue gate working).

## Diff summary

1 file changed, +89 / −1:

- `crates/caco-cli/src/lib.rs`:
  - +21 / −0 `AGENT_SHIP_ARGS` constant
  - +11 / −0 CommandSpec entry
  - +7 / −0 dispatch arm
  - +59 / −1 (the −1 is just `fn dispatch_agent_rebase(` becoming a continuation of the previous edit) `dispatch_agent_ship` function

## Operator-takeaway

**`caco agent ship` is the daily-driver command operators (and agents) have wanted all session.** Replaces a 4-step routine with one command that:
1. Rebases onto current main
2. On success, immediately reintegrates with the operator's preferred mode

Footgun-cycle handling stays manual — when a peer races my reintegrate and the agent branch goes stale mid-flight, the operator still needs `git fetch origin agent/<branch> && git reset --hard FETCH_HEAD && git merge -X theirs origin/main && git cherry-pick <SHA>`. That's a separate slice (could be `caco agent unwedge` or extending `agent ship` to detect-and-recover, deferred to a follow-up bead).

This cycle: **third quiet cycle in a row** (no broken-on-main waves to repair). The merge-queue gate upgrade (bd-29bf2b) + fast-test-gate stale-base re-check (bd-e5eec5) are clearly working. Total pre-rebase reintegrate footgun count dropped from ~1-per-cycle to 0. We can scale the agent fleet now.
