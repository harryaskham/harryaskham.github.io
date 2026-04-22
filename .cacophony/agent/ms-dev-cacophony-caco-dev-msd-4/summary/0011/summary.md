# Summary 0011 — bd-ecf1a0: trim /api/v1/ui/snapshot beads + agents

## Bead
bd-ecf1a0 (P2, bug) — `/api/v1/ui/snapshot` ships 4.2 MB JSON
(3 MB beads, 700 KB agents) on every page load and every 60s;
server takes ~3.7s to assemble.

## Approach

Per the bead's suggested fixes 1+2: trim the snapshot's bead and
agent inventories to interactive entries that the dashboard's default
"Active" filter actually renders. Closed/draft beads and old terminal
agents become lazy fetches via the existing `/api/v1/beads/all` and
`/api/v1/agents` endpoints.

Slices 3 (ETag/304), 4 (tab-hidden client backoff), and 5 (server-side
memoisation) are clean follow-ups; this bead's core acceptance is the
payload-size + build-time win.

## Change

`crates/caco-daemon/src/ui_stream.rs`:

- New pure helper `trim_snapshot_beads(beads)`: keeps only beads with
  status in {`open`, `in_progress`, `permanent`}. Drops drafts
  (~1100/2300 in current cluster) and closed (~1000/2300).
  Computed-blocked beads remain `open` in storage so they pass through.

- New pure helper `trim_snapshot_agents(agents, recent_cutoff)`: keeps
  any agent that is non-terminal, OR is a synthetic `queued` row, OR
  is terminal-but-ended within `recent_cutoff`. Default cutoff in the
  handler is 24h. Drops the long tail of completed/failed/discarded
  agents (208 → ~30 typical).

- `handle_ui_snapshot_inner` calls both helpers right before the final
  snapshot assembly. `bead_stats` is computed BEFORE the trim, so
  per-status totals on the dashboard remain accurate; only the verbose
  list payload shrinks.

- Synthetic queued spawn rows pass through unconditionally because
  the dashboard renders a dedicated Queued section.

`crates/caco-daemon/src/beads.rs`:
- Drive-by fix to test fixture `set_peer_unreachable`: added missing
  `peer_version: None` field to the `PeerReachability` struct literal
  (was breaking `cargo test --tests -p caco-daemon` on main, blocking
  any agent in this crate). Confirmed broken on main via stash check.

## Tests

`crates/caco-daemon/src/ui_stream.rs::tests::`:

- `trim_snapshot_beads_keeps_open_in_progress_permanent` — 6-bead
  fixture (`open`, `in_progress`, `closed`, `draft`, `permanent`,
  `deleted`) → keeps the right 3.
- `trim_snapshot_beads_empty_list_yields_empty` — boundary.
- `trim_snapshot_agents_keeps_non_terminal_and_recent_terminal` —
  7-agent fixture (running / queued / recent-completed /
  old-completed / old-failed / discarded-no-ended / stopped-recent)
  with 24h cutoff → keeps the right 4.
- `trim_snapshot_agents_synthetic_queued_always_passes` — guard
  against a future-dated cutoff regression that would drop queued
  spawns.

## Verification

- `cargo check -p caco-daemon` — clean.
- `cargo test -p caco-daemon --lib trim_snapshot` — 4/4 green.
- `cargo test-small` — 4202/4202 green
  (198+109+720+291+18+2814+52 across small-suite crates).

## Operational impact

- Snapshot bead payload: ~3 MB → expected ~150 KB raw (the ~94
  active beads on the cluster vs. the previous 2327-entry dump).
- Snapshot agent payload: ~700 KB → expected ~150 KB raw (the ~30
  recent agents vs. the previous 208-entry dump).
- Total payload should fall well under the 500 KB raw / 100 KB
  gzipped p95 acceptance bound. Build-time win is proportional to
  bead/agent count reduction.
- Per-status counts in `bead_stats` (used for tab badges) are
  computed BEFORE the trim and remain accurate.
- No regression to Active-filter Beads tab or Active-filter Agents
  tab rendering — both consume only the trimmed subset already.
- Lazy fetches: dashboards that want the full inventory still have
  `GET /api/v1/beads/all` and `GET /api/v1/agents` available
  on demand.

## Deferred (clean follow-ups)

- ETag / Cache-Control + If-None-Match → 304 fast-path (slice 3).
- Tab-hidden client backoff via `document.visibilityState` (slice 4).
- Server-side memoisation: build snapshot once per N seconds and
  serve identical bytes (slice 5).

These are now small wins on top of the main payload reduction. Will
file as follow-up beads if not already filed.

## Next

Reintegrate direct, close bd-ecf1a0, file follow-up beads, idle.
