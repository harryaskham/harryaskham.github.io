# Session summary — caco agent backfill-short-names (bd-8eaedf)

## Goal

Follow-up to bd-34d0b8. The lifecycle fix lands new agents with their resolved adj_noun label, but every agent already on disk shipped with `short_name: null`. Add a one-shot backfill command (and a daemon endpoint) so the operator can retroactively name the existing fleet.

## Bead(s)

- `bd-8eaedf` — [bd-34d0b8 follow-up] caco agent backfill-short-names

## Before state

- After bd-34d0b8 landed, only newly-spawned agents got short_names. The 173 pre-existing agents on disk remained null.
- No CLI verb existed to fix them in bulk; the operator would have to call `caco agent rename --id X --name Y` per agent.

## After state

- `cargo test -p caco-daemon --lib backfill_short_names` — 3 / 3 passed.
- `cargo test-small` — 197 / 109 / 718 / 286 / 18 / 2805 / 48 passed, 0 failed.
- `cargo check --workspace --tests` — clean.
- `caco agent backfill-short-names --help` works; `--dry-run / --regenerate / --project` all wired.

## Diff summary

- Commit: `394c056a`
- Files touched:
  - `crates/caco-daemon/src/lib.rs` — new `handle_agents_backfill_short_names` + route registration on both router setups; 3 new unit tests.
  - `crates/caco-cli/src/lib.rs` — new `AGENT_BACKFILL_SHORT_NAMES_ARGS`, CommandSpec, dispatch arm, and `dispatch_agent_backfill_short_names` HTTP client with pretty + JSON output.
- Tests: +3 unit; 0 removed; 0 flipped.
- Behavioural delta: opt-in operator command. Without invocation, nothing changes. With invocation, agents whose project has a strategy get a label; agents without a configured strategy get a `no_strategy_configured` skip reason in the response.

## Operator-takeaway

To retroactively name the fleet:
  - `caco agent backfill-short-names --dry-run` to preview cluster-wide proposals.
  - `caco agent backfill-short-names --project cacophony` to apply for one project.
  - `caco agent backfill-short-names --regenerate` to force a re-roll even on agents that already have a name (rare).

The handler intentionally consults only `agent_defaults.short_name_strategy` — not per-persistent-decl overrides — because backfill is the operator-facing "apply project default to whatever is null" verb. If anyone needs per-decl backfill it can be a tiny follow-up. The daemon currently restarts every few minutes on this fleet; backfill is safe to run again after each restart since named agents are skipped without `--regenerate`.
