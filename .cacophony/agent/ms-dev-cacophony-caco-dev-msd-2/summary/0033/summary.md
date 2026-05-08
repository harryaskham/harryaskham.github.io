# Session summary — Diagnose recurring TTS spoken-name lookup failures

## Goal

Make the TTS daemon's `daemon_beads_all` spoken-name refresh failure actionable after a post-`bd-9552ca` recurrence: if the narrow beads lookup fails while the daemon is otherwise healthy, the log/status detail should say so rather than only reporting a generic request error.

## Bead(s)

- `bd-438fa6` — TTS daemon_beads_all failures recur after bd-9552ca close

## Before state

- Failing tests: none known for this focused path.
- Relevant metrics: log-monitor saw `on-demand spoken-name refresh failed (retry in 900s)` for `source=daemon_beads_all endpoint=http://127.0.0.1:11100/api/v1/beads/all?limit=5000`, while `caco status` and `caco service status` were OK/healthy in the same sweep.
- Context: `bd-9552ca` had added source-chain/transport detail, but a failing `/api/v1/beads/all` lookup still did not include a lightweight local-daemon health probe to distinguish endpoint-specific routing failures from a dead local daemon.

## After state

- Failing tests: none observed in focused validation.
- Relevant metrics: queued focused validation passed in `tj-54a15157` (`RUST_MIN_STACK=33554432 cargo test -p caco-cli tts_daemon_spoken_name_beads_lookup -- --nocapture`).
- Context: lookup transport and HTTP failures now append a bounded `/api/v1/node` probe result with endpoint/status and node/state/body or probe transport detail. Existing backoff, quiet retries, and recovery confirmation behavior remain unchanged.

## Diff summary

- Commits: `f0ee53fa96`
- Files touched: `crates/caco-cli/src/lib.rs`, `SPEC.md`, `README.md`, `AGENTS.md`
- Tests: updated the existing TTS spoken-name transport and HTTP 503 tests to assert `daemon_probe=` detail and a healthy probe response.
- Behavioural delta: a recurring `daemon_beads_all` failure now records whether the local daemon probe was healthy, giving operators a bounded degraded reason and preserving automatic recovery after the existing cooldown.

## Operator-takeaway

The TTS daemon still treats spoken-name lookup as best-effort, but failures now carry enough local-daemon evidence to tell whether the problem is the beads endpoint/proxy path versus the daemon being down, without requiring a daemon or TTS restart to recover.
