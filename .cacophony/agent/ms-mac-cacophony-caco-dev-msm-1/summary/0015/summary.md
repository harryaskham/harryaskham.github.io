# Session 0015 — bd-262bd5 (caco doctor --top N)

## Goal

Slice 1 of `caco doctor` self-diagnostics enhancement. The dispatcher
already exists with rich check coverage (config / auth / pki / daemon /
services / mesh / projects / state / storage / runner / runtime / ci /
errors / version) and `--suggest` for dry-run remediation. This slice
ships:

- Convert `caco doctor` from `mcp_leaf` (no documented args) to a full
  `CommandSpec` exposing `--suggest`, `--top N`, `--node` so MCP and
  agents can plan calls.
- `--top N` ranks checks by severity (critical > warning > ok) and
  caps text-mode rendering to the worst N. JSON output unchanged
  (callers want the full envelope).
- `--node` is wired but slice 1 only changes labelling; cross-node
  remote probes deferred.

Out of scope (deferred): cross-node probe RPC, `--project` filter
(parent bead asked but local probes don't need it yet), exit-code
mapping (e.g. exit 2 on critical) — file as follow-up if operator
wants it for CI gating.

## Bead(s)

- **bd-262bd5** — primary; --top + spec upgrade.

## Before state

HEAD synced to origin/main (88b2f19f post-pull). `caco doctor` was a
single-flag command surfacing all checks unranked.

## After state

- `DOCTOR_ARGS` const added near `FLEET_SNAPSHOT_ARGS`. Documents
  `--suggest` (bd-4fcf9c), `--top` (bd-262bd5), `--node` (bd-262bd5).
- `mcp_leaf("doctor", ...)` replaced with a full `CommandSpec` that
  references `DOCTOR_ARGS`. Marked `mcp_enabled = true`,
  `agent_safe = true`, `idempotent = true` to preserve current
  semantics.
- `dispatch_doctor` signature now takes `top: Option<usize>`.
- New helper `select_top_doctor_checks(checks, n)` does a stable
  rank-by-severity sort and slices to `n`. Stable so within-severity
  area grouping preserves caller order.
- Text-mode renderer applies the helper to produce
  `display_checks: Vec<DoctorCheck>` before the per-area loop. JSON
  output remains unfiltered.
- Routing in the top-level dispatcher parses `--top` (rejects 0 and
  non-integer values with structured CliError).
- Two existing test sites updated to pass `None` for the new `top`
  parameter.

## Tests

- New: `doctor_select_top_returns_critical_first_then_warning_then_ok`
  (unit) — locks the rank function: critical > warning > ok, stable
  on ties, asks-more-than-available returns full set.
- New: `doctor_help_json_advertises_top_and_suggest_and_node`
  (help-json contract) — all three flags must appear so MCP
  tooling sees them.
- All 11 existing doctor tests still pass.

## Validation

- `RUST_MIN_STACK=33554432 cargo test -p caco-cli --lib doctor_`:
  11/11 PASS (3 mine + 8 pre-existing including `doctor_help_available`,
  `doctor_includes_lifecycle_supervisor_section`,
  `doctor_surfaces_missing_import_warnings`).
- `cargo test-small`: 4244+ tests across 8 binaries, 0 failures.
- `cargo clippy -p caco-cli -p caco-daemon -p caco-beads -p caco-web
  --all-targets -- -D warnings`: clean.

## Diff summary

```
crates/caco-cli/src/lib.rs                       | ~+150 / -10
.cacophony/agent/.../summary/0015                | (new)
```

## Operator-takeaway

```bash
caco doctor                # full report (unchanged)
caco doctor --top 5        # only the 5 worst issues, critical first
caco doctor --top 3 --suggest   # top 3 + dry-run remediation block
caco doctor --json | jq '.checks | map(select(.status != "ok"))'
                            # JSON envelope unchanged; do your own ranking
```

The CI-gating exit-code remap (e.g. `--strict` → exit 2 if any
critical) is not in this slice; if needed, file a follow-up. The
cross-node probe RPC needed for `--node remote` to actually test the
remote daemon is also deferred — slice 1 only labels the report.

## Coordination

- Spoke claim with planned scope.
- Will speak completion + reintegrate.
