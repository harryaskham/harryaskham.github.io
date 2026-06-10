# bd-cc0b06 — Frozen-WAL ops row: DaemonState recording + /api/v1/beads/wal-status + caco ops finding

## Bead
bd-cc0b06 (beads/daemon-resilience/wal, P2; filer po4-1) — slice-2 follow-on. Slices 1 (predicate, store.rs) + 2 (sampler + emit_frozen_wal_alert loop, msm-3 d85296340) landed. This slice implements the remaining **dedicated caco ops frozen-WAL ROW** ask: record the sampler's per-project frozen state into DaemonState + surface it proactively in the controller smooth-ops view (beyond the one-shot Critical alert). Coordinated with msm-3 (slice-2 author, confirmed it's mine, no file overlap — they're on bd-465587 in handle_agent_stop/lifecycle.rs).

## Change

### crates/caco-daemon/src/lib.rs
- `DaemonState.frozen_wal_status: Arc<Mutex<HashMap<String, FrozenWalStatusRow>>>` (mirrors `planned_outages`) + init at all 14 constructor sites.
- `FrozenWalStatusRow { project, detected, db_size_bytes, wal_size_bytes, since, sampled_at }` (Serialize) + pure `next_frozen_wal_row(...)` helper: preserves the rising-edge `since` across an ongoing frozen episode, clears it on recovery, and stamps a fresh `since` on re-freeze.
- The existing "beads frozen-wal detection loop" now records the current per-project status every sample (using `obs.detected` from `FrozenWalObservation`) into the map — no change to the rising-edge `newly_detected` alert path.
- `GET /api/v1/beads/wal-status` (`handle_beads_wal_status`): read-only, node-local; returns `{ count, frozen_count, rows }`. Route on the local-bearer router next to `/api/v1/links`; explicit auth allowlist entry.

### crates/caco-cli/src/ops_cmd.rs
- `collect_frozen_wal(project, ...)` collector: fetches `/api/v1/beads/wal-status`, scopes rows to the ops project.
- Wired as a concurrent `spawn_ops_collector("frozen_wal", ...)` into `build_ops_snapshot` (recv list, `take_collector`, `OpsFindingInputs.frozen_wal`, `inputs.frozen_wal`).
- `plan_findings` emits a `beads.frozen_wal` finding: **Blocked** when any project's WAL is frozen (carrying the EXTERNAL recovery runbook — `caco down` → checkpoint-on-clean-stop → `caco up`; never an in-process forced checkpoint, po4-1 caveat 1), else Routine. Also added to the degraded-collector list.

## Scope / deferred
- **caco doctor row** (the other half of "ops / doctor row"): a follow-up sub-slice — a `DoctorCheck` reading the same endpoint. Filed separately so this slice stays bounded. The ops row is the bead's stated primary value ("proactively surface the state in the controller smooth-ops view").
- **Slice 3 (auto-checkpoint/recovery)**: stays deferred per po4-1 caveats 1+2 (the daemon holds the WAL, so an in-process forced checkpoint is the exact failing op).

## Validation (daemon test queue)
- `cargo test -p caco-daemon --lib bd_cc0b06` (tj-0c56634f): PASSED — `next_frozen_wal_row_tracks_episode_since_bd_cc0b06` (rising edge / ongoing / recovery / re-freeze `since` tracking). daemon clippy 0 warnings (tj-1b22d6cb).
- `cargo test -p caco-cli --lib cc0b06` (tj-63b2272b): PASSED — `frozen_wal_finding_blocks_when_detected_bd_cc0b06` (Blocked when detected + runbook in summary; Routine when clear). cli clippy 0 warnings (tj-7ed8d689).
- Changed regions rustfmt-clean (skip_children); `git diff --check` clean.

End-to-end live frozen-WAL firing (bead caveat 3) wants a live repro; the recording + endpoint + finding classification are unit-tested headless.

## Diff
See the reintegration receipt for the landed squash SHA.
