# Session summary — bd-acdb13 daemon fleet snapshot history

## Goal

Advance the broader state snapshot system by extending the existing daemon-owned same-host fleet snapshot cache from a single last-known-good artifact into an LKG plus bounded timestamped history of recent fleet snapshots.

## Bead(s)

- `bd-acdb13` — Build state snapshot system for cached data

## Before state

- A daemon-authored `daemon/state/fleet-snapshot-lkg.json` LKG read-model artifact already existed for same-host stale bootstrap/fallback.
- The LKG was atomically written, schema/versioned, bounded per-file, and loadable as an offline/stale cache.
- There was no daemon-owned local history of recent full/fleet snapshots; only the latest artifact survived.
- Mobile/watch relayed-state snapshot slices had already landed separately; this slice stays in daemon/server generic state storage and does not touch mobile surfaces.

## After state

- The daemon now writes both:
  - latest LKG: `daemon/state/fleet-snapshot-lkg.json`
  - bounded history: `daemon/state/fleet-snapshots/<timestamp>-<node>-<uuid>.json`
- History writes reuse the same serialized `FleetSnapshotReadModel` payload as the LKG, so schema/version/source/freshness metadata stays aligned.
- History filenames sanitize the origin node and include timestamp + UUID to avoid collisions.
- History retention is bounded after each write:
  - max count: `FLEET_SNAPSHOT_HISTORY_MAX_COUNT = 24`
  - max aggregate bytes: `FLEET_SNAPSHOT_HISTORY_MAX_BYTES = 64 MiB`
- Non-JSON files are ignored by pruning; stale/over-cap JSON files are removed best-effort without failing the UI snapshot response path.
- `GET /api/v1/ui/snapshot` response semantics remain unchanged; persistence remains best-effort and offloaded to `spawn_blocking`.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched:
  - `crates/caco-daemon/src/ui_stream.rs`
  - `SPEC.md`
  - `README.md`
  - `AGENTS.md`
- Behavioural delta:
  - added `fleet_snapshot_history_dir` and retention constants.
  - split fleet snapshot serialization and atomic write helpers for reuse by LKG + history.
  - added `persist_fleet_snapshot_read_model_with_history` and `prune_fleet_snapshot_history`.
  - wired `/api/v1/ui/snapshot` best-effort persistence to write LKG + bounded history.
  - updated operator/docs contracts to mention latest + retention-bounded history artifacts.

## Validation

- `cargo test -p caco-daemon fleet_snapshot --lib` passed.
- `cargo check -p caco-daemon --lib` passed.
- `cargo clippy -p caco-daemon --lib -- -D warnings` passed.
- `./scripts/rustfmt-changed.sh crates/caco-daemon/src/ui_stream.rs` formatted the touched Rust file.
- `cargo check -p caco-daemon --lib` passed again after formatting.
- `cargo clippy -p caco-daemon --lib -- -D warnings` passed again after formatting/docs alignment.

## Operator-takeaway

The daemon now preserves a bounded recent history of same-host fleet snapshot read-model artifacts in addition to the latest LKG cache, enabling warm-start/debugging of recent project/agent state without making UI snapshot HTTP/SSE semantics or browser access depend on local files.
