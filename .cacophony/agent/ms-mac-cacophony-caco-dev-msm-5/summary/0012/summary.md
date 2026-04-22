# Session summary 0012 — bd-2b7a37 slice 2: wedge_severity tier

## Goal

Add a severity-tier classifier and `[SEVERE]` doctor badge to the
`potentially_stuck` agents area so operators can triage stuck entries
at a glance without having to do the idle/threshold math themselves.

## Bead(s)

- `bd-2b7a37` — Post-restart agent handoff stuck pattern. Slice 2 of
  multi-acceptance bead (slice 1 = doctor surfacing, landed earlier;
  slice 3 = auto-recreate, deferred per below).

## Before state

- `potentially_stuck` entries in `caco doctor` showed raw idle values
  but no tier. Operators had to compute "is 1800s idle vs 60s
  threshold severe or mild?" by hand for every line.
- Effectively only one bucket: present-in-list = "you decide".

## After state

- New per-entry fields in `agents/summary` JSON output:
  - `wedge_severity`: `"severely_wedged"` if idle > 2x threshold,
    else `"mildly_wedged"` (covers both 1x-2x and unknown-idle).
  - `time_past_stale_secs`: how many seconds past the threshold the
    agent has been idle (saturating subtract; never negative).
- `caco doctor` renderer: `[SEVERE]` badge appended after the agent
  ID for `severely_wedged` entries. Both Potentially-Stuck and
  Stranded sections get the badge (severity is a property of the
  idle math, not of node attribution).
- 2 unit tests:
  - `agents_summary_includes_last_tool_activity_age_and_threshold`
    (extended): 700s idle vs 60s threshold → `severely_wedged`,
    `time_past_stale_secs >= 640`.
  - `agents_summary_marks_mildly_wedged_when_idle_under_2x_threshold`:
    90s idle vs 60s threshold (1.5x) → `mildly_wedged`.

## Diff summary

- Commit: `8f856df2`.
- Files: `crates/caco-daemon/src/lib.rs` (+severity calc + 1 new
  test, extended 1 existing), `crates/caco-cli/src/lib.rs`
  (+badge rendering).
- Tests: +1 new, +1 extended; both pass.
- `cargo build -p caco-daemon -p caco-cli`: clean.
- `cargo clippy -p caco-daemon -p caco-cli --tests`: clean (only
  pre-existing unrelated warnings in caco-cli).

## Out of scope (deferred)

- **Acceptance #3 (auto recreate-or-fail)**: the destructive_relaunch
  path itself is invasive and false-positives cost a workspace
  rebuild. Better to let operators triage on the new badge for one
  cycle so we have field data on which `severely_wedged` entries
  self-recover vs which truly need recreate. Then an automated
  threshold can be tuned without overshoot.
- **TUI/web/android surfacing** of the severity field — multi-surface
  follow-up. CLI/daemon ships now.

## Operator-takeaway

Run `caco doctor` and look for `[SEVERE]` badges in the
Potentially-Stuck section. Any persistent agent flagged with
`[SEVERE]` past 2x its 1800s idle threshold (~1h+ idle) is the
bead's "manual intervention required" signal — that one's not coming
back without a `caco agent recreate`. Workers without the badge will
likely self-recover on the next heartbeat or handoff cycle.
