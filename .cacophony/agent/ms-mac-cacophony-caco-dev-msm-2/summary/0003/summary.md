# Session summary — short_name dropped + missing from list JSON (bd-34d0b8)

## Goal

The bead asks for "ability to rename agents in TUI / android / webapp" AND "work out why many agents do not end up with an assigned shortname automatically". The CLI verb (`caco agent rename`) and the daemon `field/set` endpoint already exist; the dispatching UI surfaces are filed as separate follow-ups (bd-09e8df / bd-3ae0c6 / bd-3cf67f). The unanswered question — *why are short_names so often unset even when adj_noun is configured* — turned out to be the load-bearing slice for this session, and fixing it required also surfacing the labels in list JSON so any UI can read them.

## Bead(s)

- `bd-34d0b8` — Ability to rename agents in the TUI / android / webapp; investigate missing short_names

## Before state

- Project `cacophony` has `agent_defaults.short_name_strategy: { strategy: adj_noun }`.
- `caco agent list --json` over 173 live agents returned `0` with `short_name` populated.
- `caco agent get --id $CACO_AGENT_ID --field short_name` returned `null` for the persistent agent this very session was running under, even though `resolve_short_name` is called in the create path.
- `agent_list_row_json` did not include `short_name` / `spoken_name` / `agent_name` keys at all in its output.
- `SnapshotAgentRow` had no fields for these labels, so even if local rows surfaced them, peer rows would not.

## After state

- `cargo test -p caco-daemon --lib agent_list_row_json` — 2 / 2 passed (new tests).
- `cargo test-small` — 197 / 109 / 718 / 286 / 18 / 2797 / 45 passed, 0 failed.
- `cargo check --workspace --tests` — clean.
- New agents launched on `cacophony` will arrive with their resolved `short_name` populated; list JSON exposes the three label fields (and emits null, not absent, when unset).

## Diff summary

- Commit: `4729abed`
- Files touched:
  - `crates/caco-daemon/src/agent/lifecycle.rs` — fix the dropped `req.short_name` in the success-path `AgentInfo` literal (`line 1736`). This is the root bug.
  - `crates/caco-daemon/src/lib.rs` — add `short_name`, `spoken_name`, `agent_name` to `agent_list_row_json()` and to the two peer-snapshot merge sites (`/api/v1/agents` and `/api/v1/projects/{p}/agents`). Two new unit tests.
  - `crates/caco-daemon/src/replication.rs` — `SnapshotAgentRow` gains the three label fields (with `#[serde(default, skip_serializing_if = "Option::is_none")]` for forwards compatibility with peers that pre-date the change). `build_snapshot_agents` populates them.
  - `crates/caco-daemon/src/{beads,ui_stream}.rs` and `tests/{daemon,multinode}.rs` — 38 `SnapshotAgentRow` test literals updated to include the new fields. Mechanical.
- Tests: +2 unit; 0 removed; 0 flipped.
- Behavioural delta: persistent and worker agents that go through the create path now persist their resolved short_name. List endpoints surface the three label fields on every row (null when unset).

## Operator-takeaway

After this lands, every newly-spawned agent on a project with `short_name_strategy` configured will get its `short_name` populated and visible in `caco agent list --json` and `/api/v1/agents`. **Existing agents already on disk will not retroactively get names** — they'd need a one-shot reassignment pass (a small follow-up: `caco agent backfill-short-names` or a daemon migration). I deliberately did not write that migration in this commit because the operator may want to choose between "regenerate everywhere" vs "leave the historical fleet alone and only name new agents". The TUI / web / android rename UX follow-ups (bd-09e8df / bd-3ae0c6 / bd-3cf67f) now have a real `short_name` field to read and write against; before this they would have been wired to a permanently-null surface.
