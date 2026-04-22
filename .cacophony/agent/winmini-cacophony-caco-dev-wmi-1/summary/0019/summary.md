# Session summary — Persistent idle auto-restart observability (bd-d3e2c5)

## Goal

Log-monitor sweeps 25/28 flagged caco-ctrl persistent
hitting `idle-auto-restart-fail` 3+ times in ~1h. The
auto-restart loop in `auto_restart_idle_persistent_agents`
already logged a warn on attempt and routed Err(e) to the
structured-log error channel, but:

- No stderr line on failure → invisible in `journalctl`
  / console output without a feed reader.
- No success-side eprintln → operators couldn't see the
  full attempt → outcome cycle when sweeping.
- The structured-log `detail` was just `e.to_string()` →
  no idle/threshold/project context for diagnosis.

bd-d3e2c5 asked for "structured logging to auto-restart
loop so retries are visible rather than just supervisor-
respawn-after-fail."

## Bead(s)

- `bd-d3e2c5` — [bd-08e44d family] caco-ctrl persistent
  hits idle-auto-restart-fail repeatedly (P2 bug)

## Before state

- Auto-restart loop's failure handler routed `Err(e)` only
  to the structured-log channel with `detail = e.to_string()`
  — no idle/threshold/project context, no stderr line.
- Success path published a feed event but no stderr line.
- Result: log-monitor sweeps 25/28 detected
  `idle-auto-restart-fail` from feed events but couldn't
  see the full cycle without a feed reader; operators
  triaging from journalctl saw nothing.

## After state

- Failure path emits stderr eprintln with agent_id,
  persistent_id, project, idle_secs, threshold_secs, error.
- Success path emits stderr eprintln with agent_id,
  persistent_id, project, resume_method.
- Structured-log `detail` enriched with the same context
  fields (not just `e.to_string()`).
- `bd-d3e2c5` label added to the structured-log record so
  log-monitor sweeps can filter cleanly.

## Diff summary

- Files touched (+45 / −5):
  - `crates/caco-daemon/src/lib.rs` (~13770 area):
    - Failure path: stderr eprintln with full context
      (agent, persistent_id, project, idle_secs,
      threshold_secs, error). Structured-log detail
      enriched to mirror the same fields. `bd-d3e2c5`
      label added so log-monitor sweeps can filter.
    - Success path: stderr eprintln mirroring the
      failure shape so the full cycle (warn → ok | fail)
      is observable in plain-text logs.

## Verification

- `cargo build -p caco-daemon`: clean.
- `cargo test-small`: 56 pass.
- `cargo clippy -p caco-daemon --lib -- -D warnings`:
  clean.

## Operator-takeaway

When persistent idle auto-restart fires, daemon stderr now
shows:

```
warn agent: persistent agent <id> idle for Ns (threshold Ns) — auto-restarting
bd-d3e2c5: persistent idle auto-restart OK for agent=<id> persistent_id=<pid> project=<proj> resume_method=<m>
```

or on failure:

```
bd-d3e2c5: persistent idle auto-restart FAILED for agent=<id> persistent_id=<pid> project=<proj> idle_secs=N threshold_secs=N error=<e>
```

Log-monitor sweeps can grep `bd-d3e2c5: ... FAILED` for
direct counting, and the structured-log detail / labels
enable richer dashboards.

## Scope kept narrow

This is observability only — no retry counter, no per-agent
backoff, no circuit breaker. The bead's diagnostic-next-steps
list (capture which exact API call fails, cross-reference
with sync-500 timing, etc.) is now actionable because the
logs surface enough to identify the failing call. Follow-up
slices can decide whether (a) backoff, (b) retries, or
(c) bd-08e44d-family root-cause-fix is the next move.

## Drive-by

(none — single-file focused change)
