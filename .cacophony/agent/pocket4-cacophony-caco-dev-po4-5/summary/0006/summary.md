# Session summary — `caco restart` cascades to PID-only services

## Goal

Close bd-730245 (P0): `caco restart` silently skipped caco-tts-daemon
across 19+ daemon upgrades today, causing the TTS daemon to run a
3-day-old stale binary while the rest of the cluster moved forward.
Every TTS-related fix shipped today had been verified against that
stale process, producing misleading results.

## Bead(s)

- `bd-730245` — [tts/lifecycle] TTS daemon never restarted across 19+
  daemon upgrades today

## Before state

- `dispatch_restart(None, ...)` derived
  `effective_service_filter = Some("caco-daemon")`.
- `LifecycleManager::shutdown(Some("caco-daemon"))` matched only the
  caco-daemon entry and skipped every other managed service.
- caco-tts-daemon (pid_only) stayed alive, converge's
  `is_pid_alive()` returned true, and the stale process was never
  respawned with the new launcher/binary.

## After state

- Default `caco restart` (no --service) cascades: service_filter=None
  → shutdown stops every lifecycle-managed service → converge respawns
  all from the new launcher.
- Explicit `--service caco-daemon` (or any other name) preserves the
  scoped-restart behaviour for operators who need it.
- RESTART_ARGS `--service` help text updated to document the cascade
  default and reference bd-730245.

## Diff summary

- Commit: bd-730245 caco restart cascades to PID-only services
- File: `crates/caco-cli/src/lib.rs` (+14/-2)
- Tests: 190 cargo test-small pass. Full e2e requires a running
  daemon; leaving observability check to bd-98b8d8 follow-up.

## Operator-takeaway

`caco restart` now does what operators assumed it did: restarts *every*
lifecycle-managed service on the node, not just caco-daemon. If you're
debugging TTS/STT/beads issues after a daemon upgrade, a plain
`caco restart` now actually upgrades the sidecar binaries too. If you
want the old "daemon only" behaviour, pass `--service caco-daemon`.

Credit: po4-3 pair-debugged the converge pid-alive branch in parallel
and sent a support note with precise line numbers (47927, 1013, 1023,
786) that matched my investigation, plus a second-order observation
about pid-alive + stale-binary that's worth a follow-up bead.
