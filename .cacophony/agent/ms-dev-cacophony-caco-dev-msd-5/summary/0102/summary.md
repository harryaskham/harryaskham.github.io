# Session summary — STT daemon orphan/duplicate visibility (bd-272df9)

## Goal

Overnight takeover (per Harry) of an offline-node (aurora) P1 bug: orphaned/
duplicate caco tts+stt daemons accumulate on ms-mac because the single-owner
supervision check is blind, so the supervisor respawns duplicates.

## Bead

- `bd-272df9` (P1 bug, reclaimed from offline aurora aur-5) — orphaned/duplicate
  audio daemons accumulate (single-owner supervision violation).

## Key finding (triage)

The DURABLE root-cause fix — the STT daemon atomically writing `stt-daemon.pid`
AND `stt-daemon.port` on startup so the single-owner check can see the live
owner — is **already on main**, landed incidentally by the caco-transcription
agent (commits 6c99dbfbab / 727c1a4fd6), NOT by this bead. So a clean restart no
longer re-accumulates orphans (the doctor's pinned respawn mechanism is resolved).

## After state (this slice)

- Added a **detection-only** orphan/duplicate VISIBILITY diagnostic to `caco stt
  daemon status` (acceptance #2 "surfaced as a diagnostic" + #3 "make duplicate/
  orphan visible"): a pure `untracked_stt_daemon_pids(ps, instance, tracked_pid)`
  filter + a bounded `ps` enumeration. It lists untracked `stt daemon` pids for
  the instance, excluding the tracked owner, `audio transcribe --live` capture
  children, and one-shot status/logs commands. **Never reaps** — killing the live
  owner or a warming starter is exactly the risk this avoids. Surfaced as a
  `duplicate_processes` JSON field + a `⚠ N untracked/orphan stt-daemon
  process(es)` suffix on the text status (so the doctor's "not running" + alive
  orphans scenario is now visible).

## Split (coordinated with ctrl)

- This CODE (visibility diagnostic) is mine, landed through the gated ms-dev path.
- The LIVE operational cleanup (kill the currently-accumulated ms-mac dupes) is
  the ms-mac worker's complementary peer-lane step (no bead/code edits).
- bd-272df9 closes once this lands + ms-mac confirms live dupes cleared.

## Diff summary

- Code commit: `bd-272df9: STT daemon orphan/duplicate visibility diagnostic`.
  Final landed squash SHA from the reintegration receipt.
- File: `crates/caco-cli/src/audio_cmd.rs` (helpers + status wiring + test).
- Test: `cargo test -p caco-cli --lib untracked_stt_daemon_pids` (pass).

## Operator-takeaway

`caco stt daemon status` now shows orphan/duplicate stt-daemon pids instead of
hiding them behind a blind "not running". The durable respawn-prevention (atomic
pid+port write) was already on main; the live ms-mac dupes are an operational
cleanup owned by the ms-mac worker.
