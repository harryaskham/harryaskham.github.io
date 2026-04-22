# Session summary — bd-84c645 persist active mode state

## Goal

Make the daemon's active mode selection (global mode + per-project
overrides) survive `caco restart`, crashes, and auto-restart
upgrades, so operator-set burndown / triage modes actually keep
firing rules instead of silently reverting to manual.

## Bead(s)

- `bd-84c645` — Active mode state (global + per-project overrides)
  not persisted across daemon restarts.

## Before state

- `ActiveModeState` was constructed via `Default` on every daemon
  start (`global_mode = "manual"`, empty overrides).
- The mode-execution loop short-circuits on `manual`, so any
  burndown / triage mode set via TUI or HTTP was silently lost on
  restart and no rules fired.
- No persistence layer existed; no `load`/`save` methods; no path
  in `RuntimePaths`.

## After state

- `RuntimePaths` gains `active_mode_state` =
  `$CACOPHONY_DIR/daemon/active-mode.json`.
- `ActiveModeState` gains three methods:
  - `load(path)` — reads JSON; returns `None` for missing or
    corrupt files (warns to stderr, never fails daemon startup).
  - `drop_unknown_modes(available)` — strips persisted mode names
    no longer in the config's `modes:` map. Global reverts to
    `manual`; per-project overrides are removed; `"manual"` is
    always treated as valid even when not explicitly listed.
  - `save_atomic(path)` — writes `<path>.tmp` then renames into
    place so `kill -9` mid-write leaves either the old or new
    version, never partial. Creates parent dir if needed.
- Canonical `DaemonState` construction calls
  `ActiveModeState::load(...).unwrap_or_default()`,
  `drop_unknown_modes(config.available_modes())`, and logs
  `bd-84c645: restored active mode state: global=X, overrides=...`
  on startup.
- All three mutation handlers
  (`handle_modes_set_global`, `handle_modes_set_project`,
  `handle_modes_clear_project`) now call `save_atomic` after
  mutating; a save failure is logged to stderr but does not fail
  the HTTP request.

## Diff summary

- Commits: `83dc3057`
- Files touched:
  - `crates/caco-config/src/paths.rs` (+1 field, +1 init line)
  - `crates/caco-daemon/src/modes.rs` (+ load / drop_unknown_modes
    / save_atomic + 6 unit tests)
  - `crates/caco-daemon/src/lib.rs` (+ initial_mode_state load +
    save_atomic on 3 mutation handlers)
- Tests: +6 / -0 / flipped 0
- Behavioural delta: setting global mode or per-project override
  now survives daemon restart; unknown persisted modes log a
  warning and revert to manual; partial-write corruption is
  impossible under crash.

## Validation

- `cargo test -p caco-daemon --lib modes::tests::` — 28 passed.
- `cargo test-small` workspace — all suites green.

## Operator-takeaway

If burndown stops firing after a restart, check
`$CACOPHONY_DIR/daemon/active-mode.json` — it should record the
last operator-set mode. The daemon log will print
`bd-84c645: restored active mode state: ...` at startup and a
warning if any persisted mode was dropped because it's no longer
in `modes:` config. Mode state remains node-local-daemon-state;
cluster-wide sync is out of scope for this bead.
