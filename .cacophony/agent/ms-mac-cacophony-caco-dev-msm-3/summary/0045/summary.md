# Session summary — bd-220adc max restart backoff env override

## Goal

Make MAX_RESTART_BACKOFF_SECS operator-tunable via env var, mirroring
bd-acfa92 pattern.

## Bead(s)

- `bd-220adc` — Persistent agent backoff cap configurable

## Before state

- 3600s hardcoded ceiling; no operator knob short of recompile

## After state

- `CACO_MAX_RESTART_BACKOFF_SECS` env var override; bit-for-bit
  fallback to 3600s default when unset/unparseable/zero
- 2 new tests guard the override + fallthrough paths
- Per-agent override + saturation telemetry deferred as follow-ups

## Diff summary

- Commits: 309289ff3cef
- Files: `crates/caco-daemon/src/persistent.rs`
- Tests: +2

## Operator-takeaway

Two beads in this session (bd-acfa92 + bd-220adc) applied the same
env-var pattern to "magic constant" friction. Worth canonicalising as
a helper if a third callsite shows up.
