# Session summary — bd-393bd8 mode_event controller broadcast

## Goal

Give controller-role agents (caco-ctrl, cluster-ctrl, per-project
controllers) a live signal of every burndown / mode rule firing so
operators can monitor health without polling `caco agent list` or
`caco bd list --status claimed`.

## Bead(s)

- `bd-393bd8` — Broadcast burndown mode events
  (spawn/claim/reject/cooldown) to controller agents for live
  monitoring.

## Before state

- `mode_execution_loop` only emitted human-readable `eprintln!`
  log lines and the existing `ModeAction` feed event.
- No mechanism for controllers to react to per-tick mode firings;
  monitoring required polling agent / bead state on a timer.
- Cooldown- and concurrency-skipped rules were invisible outside
  the daemon log.
- Spawn failures left only the synthetic feed event (now
  `ModeSpawnFailed` after bd-102eb0); no controller-targeted
  message.

## After state

- New helper `modes::emit_mode_event_broadcast` publishes a
  `Message::broadcast` with `visibility="controllers_only"` and a
  structured JSON body:
  ```
  { "kind": "mode_event",
    "mode": "burndown",
    "project": "cacophony",
    "node": "helsinki",
    "outcome": "executed" | "skipped_cooldown" |
               "skipped_concurrency" | "skipped_other" |
               "skipped_runtime" | "failed",
    "detail": { rule_index, action_type, params,
                skip_reason | error, ... } }
  ```
- Mode loop wires the helper in two places:
  - **Pre-execute**: scan `eval_result.evaluations`, broadcast
    every matched-but-skipped rule with a label parsed from its
    `skip_reason` (`cooldown:` / `concurrency:` / other). Catches
    chronically-cooled-down rules (saturated max_concurrency)
    even on ticks where nothing fires.
  - **Post-execute**: broadcast the `Executed` / `Skipped` /
    `Failed` outcome with rule_index, action type, resolved
    params, and outcome-specific detail. `Noop` is intentionally
    excluded to keep volume proportional to interesting events.
- Worker-role agents do not see these broadcasts thanks to the
  bd-03a2b6 visibility filter; controllers and operators do.
- Envelope construction extracted to `pub(crate)
  build_mode_event_envelope` for direct unit-testing of the JSON
  shape without standing up a DaemonState.

## Diff summary

- Commits: `7201fbda`
- Files touched: `crates/caco-daemon/src/modes.rs` (+213)
- Tests: +4 / -0 / flipped 0 (modes::tests::* now 37 passing).
- Behavioural delta: every burndown firing, skip-due-to-safety,
  spawn failure, or runtime skip now produces a controller-only
  inbox message that monitors can subscribe to and react on.

## Validation

- `cargo test -p caco-daemon --lib modes::tests::` — 37 passed.
- `cargo test-small` workspace — all suites green
  (195 + 109 + 716 + 277 + 18 + 2781 + 45).

## Operator-takeaway

Controller agents can now `caco msg inbox` and filter on
`kind=mode_event` (parse the body JSON) to see every mode tick.
A controller that sees repeated `outcome=skipped_concurrency`
without intervening `outcome=executed` knows the cap is wedging
the rule; a controller seeing repeated `outcome=failed` for the
same rule_index can correlate with the bd-102eb0
`mode_failure_streak` notification. Cluster-level dashboards can
reconstruct burndown's per-tick decisions from this stream alone.
