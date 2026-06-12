# bd-34eb64 — first-party reap path for untracked orphan agent dirs (caco prune orphaned-dirs)

## Bead
bd-34eb64 (agent-lifecycle/daemon-resilience/disk-io/housekeeping, P3; filer caco-ctrl). Failed-create agent dirs (e.g. ms-mac 86r5xxkv7fxgjndx, ~1.3G, agent.json + .caco-alive/.caco-ready present, created Jun 10) are UNTRACKED by the daemon: `caco agent status --id` 404s, `caco prune run` skips them (retention sweep only covers registry-tracked dirs), and `caco agent discard` 404s. So they sit as dead disk indefinitely with no first-party reap path.

## Fix (crates/caco-cli/src/lib.rs) — new `caco prune orphaned-dirs`
Mirrors the existing `caco prune orphaned-persistents` (bd-57e873) surface: dry-run by default, `--delete` for the destructive opt-in, `--project` filter, plus `--min-age-hours` (default 6).
- Pure `is_untracked_orphan_dir(in_tracked_set, has_agent_markers, age_secs, min_age_secs, tmux_alive, process_alive)` predicate — unit-testable, the reap decision in one place.
- `orphan_dir_stored_pid(agent_dir)` — best-effort PID read from agent.json (None for the unparseable failed-create case).
- `dispatch_prune_orphaned_dirs`: raw-scans `<agents>/<project>/<id>/`, computes the tracked set from `scan_agents_dir_all` (parseable agent.json), and flags dirs that have agent markers (agent.json / .caco-alive / .caco-ready / init.sh) but are NOT in the tracked set, are >= the age floor, and have no live tmux session / stored process. Dry-run lists them with reclaimable bytes; `--delete` does a direct `std::fs::remove_dir_all` (the discard endpoint cannot reach an untracked dir).

### Safety
PRIMARY anchor = not-in-`scan_agents_dir_all`: a live managed agent always has a parseable agent.json and therefore appears in the tracked set, so it can never be flagged. The age floor (default 6h) protects a mid-spawn directory; the tmux + stored-process checks are defensive belt-and-suspenders. Dry-run is the default; `--delete` is the explicit destructive opt-in.

## Validation (daemon test queue, --cwd at checkout)
- `cargo clippy -p caco-cli --lib` (tj-535ae560): PASSED (exit 0) — compiles + lints clean (after fixing an initial E0382 move of the per-project-dir `project`/`id` vars inside the nested per-agent loop → cloned).
- `cargo test -p caco-cli --lib is_untracked_orphan_dir` (tj-ef01e38f): PASSED — predicate flags only stale, daemon-untracked, marker-bearing, non-live dirs (tracked → never; no markers → never; too-young → never; live tmux/process → never; at age floor → reapable).
- rustfmt-clean on changed regions; `git diff --check` clean. AGENTS.md prune section updated (orphaned-dirs alongside orphaned-persistents).

## Operator verification (env-gated, caco-ctrl)
The detection + predicate are host-verifiable headless, but the actual `--delete` reap was not exercised on a real orphan from headless winmini. caco-ctrl should run `caco prune orphaned-dirs --dry-run` on ms-mac and confirm it lists ONLY the real orphan (86r5xxkv7fxgjndx) before running `--delete` (the bd-7db412 implement-here / verify-on-env pattern). Dry-run-default + the not-in-scan + age guard make this safe to ship without prior live verification.

## Diff
See the reintegration receipt for the landed squash SHA.
