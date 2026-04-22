# Session 0016 — bd-9e836f (caco doctor --strict exit code)

## Goal

Add `--strict` flag to `caco doctor` so CI gates can fail when the
fleet is unhealthy. Maps worst-severity check to exit code:
2 (critical/error/fail), 1 (warning/warn), 0 (ok / unknown).
Default behaviour unchanged — `caco doctor` (no `--strict`) still
exits 0 regardless.

Bead self-filed as a bd-262bd5 follow-up after observing during
session 0015 that operator-on-call CI usage needs an exit-code
contract.

## Bead(s)

- **bd-9e836f** — primary (self-filed follow-up to bd-262bd5).

## Before state

`caco doctor` always exits 0. The bd-262bd5 work shipped --top N for
ranked text output but no machine-readable exit signal. CI scripts
had to grep stdout or parse JSON to detect critical issues.

## After state

- New `compute_doctor_strict_exit_code(checks)` helper: pure function
  returning 2/1/0 based on worst-severity check. Mirrors the rank
  function used by `select_top_doctor_checks` (critical/error/fail =
  level 2; warning/warn = level 1; everything else = 0).
- `dispatch_doctor` signature now returns `(String, u8)` — the second
  element is the strict-derived exit code, computed regardless of
  `--strict`. Caller decides whether to apply.
- Doctor arm of the top-level dispatcher reads `--strict` flag; if
  set, stores the strict exit code in a new outer
  `doctor_exit_override: Option<u8>` variable.
- Final `Outcome` wrap honours `doctor_exit_override.unwrap_or(0)`
  so all other commands keep exit 0 by default.
- `--strict` registered in `DOCTOR_ARGS` so help-json + MCP advertise
  the new flag.
- 2 existing test sites updated to destructure the new tuple return.

## Tests

- New: `doctor_strict_exit_code_picks_worst_severity` (unit) — locks
  the contract: ok-only → 0, any warning → 1, any critical/error/fail
  → 2 (dominates warning), unknown statuses count as 0.
- New: `doctor_help_json_advertises_strict_flag` (help-json contract).
- All 13 existing doctor tests still pass.

## Validation

- `cargo test -p caco-cli --lib doctor_`: 13/13 PASS (2 new + 11
  pre-existing).
- `cargo test-small`: 4255+ tests across 8 binaries, 0 failures.
- `cargo clippy -p caco-cli -p caco-daemon -p caco-beads -p caco-web
  --all-targets -- -D warnings`: clean.

## Diff summary

```
crates/caco-cli/src/lib.rs                       | ~+95 / -10
.cacophony/agent/.../summary/0016                | (new)
```

## Operator-takeaway

```bash
# CI gate examples:
caco doctor --strict                # exit 2 if any critical, 1 if warning
caco doctor --strict --json         # JSON envelope unchanged; exit code set
caco doctor --strict --top 5        # --top is cosmetic; exit reflects full set

# Explicit "warnings are OK, only critical fails the build" pattern:
caco doctor --strict || [ $? -lt 2 ]
```

JSON envelope is identical with or without --strict — only the exit
code changes — so existing JSON parsers don't need updating.

## Coordination

- Spoke claim with planned scope.
- Will speak completion + reintegrate.

## Notes for next time

- The `doctor_exit_override: Option<u8>` pattern is reusable: any
  command that wants a custom exit code can opt in by setting it
  before falling through to the final `Outcome` wrap. If a third
  command needs custom exit codes, consider promoting this to a
  proper `RuntimeOutcome` enum.
- `caco fleet snapshot` (bd-1b713a) is also exit-0-only and could
  use the same pattern. If operators ask for `caco fleet snapshot
  --strict`, the helper here generalises (severity is on
  `errors[]`).
