# bd-ecf1a0 — cap persistent-agent error messages in /api/v1/ui/snapshot

## Goal
Bring the snapshot payload back under the bd-ecf1a0 acceptance
target of <500 KB raw / 100 KB gzipped.

## Bead(s)
- bd-ecf1a0 (P2 bug). The previous trim slices (drafts/closed beads
  dropped, terminal-old agents dropped, ETag/304, memo cache,
  visibility-aware client refresh) had all landed but the live
  payload was still ~12 MB.

## Diagnosis
Live snapshot dump on ms-mac via /api/v1/ui/snapshot:
- beads: 237 KB (already trimmed by earlier slices).
- agents: 649 KB.
- persistent_agents: 10.8 MB.

Top offenders inside persistent_agents: three picasso-health
persistent agents (health-ambient/health-ctrl/health-dev) each
carrying a single `error_history[0].message` of ~3.5 MB. The
content was the entire stderr blob of a `git clone --shared` failure
(`unable to read sha1 file of .claude/gclaude.sh ...`). Nothing in
the snapshot path bounded the size of error strings.

## Before state
- `PersistentAgentSnapshot.error_history` and `.last_error` shipped
  full-fidelity through the wire, no per-field cap.
- One ~3.5 MB stderr blob in error_history defeats every other
  bd-ecf1a0 trim on its own.

## After state
`crates/caco-daemon/src/ui_stream.rs`:
- New `SNAPSHOT_ERROR_MESSAGE_MAX_BYTES = 2048`.
- New `truncate_snapshot_error_message(msg)` — UTF-8 boundary
  safe; appends `… [snapshot truncated, N bytes dropped (bd-ecf1a0)]`
  marker so operators see at a glance that trimming happened and
  roughly how big the dropped tail was.
- New `trim_snapshot_persistent_agent_error_messages(&mut [PA])`
  mutates in place, covering both `.last_error` and every
  `.error_history[].message`.
- Applied at the snapshot-build call site immediately after
  `build_persistent_agent_snapshots_async`.

Canonical full-fidelity error remains in PersistentSentinel on
disk; only the wire-format snapshot is trimmed.

## Diff summary
- `crates/caco-daemon/src/ui_stream.rs` (+203/-1):
  - Constant + 2 helpers above the existing trim helpers.
  - One in-place trim call inserted into the snapshot builder.
  - 4 new unit tests in the existing `ui_stream::tests` module.

## Tests
- `cargo build -p caco-daemon` — clean.
- `cargo clippy -p caco-daemon --all-targets -- -D warnings` — clean.
- `cargo test -p caco-daemon --lib truncate_snapshot_error` — 3/3 pass.
- `cargo test -p caco-daemon --lib trim_persistent_agent` — 1/1 pass.

## Operator-takeaway
After binary roll, the snapshot for nodes carrying picasso-health
(or any agent with a multi-MB stderr-bearing error) drops by
~3.5 MB per offending agent. ms-mac's snapshot is expected to fall
from ~12 MB to <1 MB. The trim is a hard ceiling of 2 KB per error
string — if a snapshot is still oversize after this, the next slice
is to bound `agents` (currently 649 KB; same approach: cap any
oversized `last_error` / log-trail fields). Bead remains open for
the next slice.
