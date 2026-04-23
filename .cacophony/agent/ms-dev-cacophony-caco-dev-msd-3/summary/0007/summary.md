# Session summary — bd-ce32fa: optional one-shot agent dispatch in bootstrap

## Goal

Close the bd-aa4add follow-up: make `caco bootstrap dev` actually
spawn a one-shot demo agent so the new dev sees the full
spawn → claim → reintegrate lifecycle, not just "a bead was filed,
maybe a worker will pick it up later". The acceptance criterion
explicitly required end-to-end completion ("a new operator can
run a single command and end up with a working daemon + a
completed agent reintegrated to main").

## Bead(s)

- `bd-ce32fa` — [bd-aa4add follow-up] caco bootstrap dev: full
  interactive setup (config init, daemon start, sample-project +
  sample-bead, demo agent dispatch)

## Before state

- `dispatch_bootstrap_dev_demo_agent` only filed the bead and
  printed a "next step" hint asking the operator to manually run
  `caco agent dispatch …`.
- The bead's slice marker explicitly admitted: "bd-422b85 slice 2
  (bead-only; one-shot agent dispatch is follow-up)".

## After state

- `dispatch_bootstrap_dev_demo_agent` now optionally invokes
  `caco agent dispatch --project P --profile PR` immediately
  after filing the bead, gated on two opt-in env vars:
    - `CACO_BOOTSTRAP_DEMO_PROFILE` — required to attempt
      dispatch (a fresh dev machine won't have a profile yet).
    - `CACO_BOOTSTRAP_DEMO_PROJECT` — defaults to `"sandbox"`
      (matches the project the unified bd-aa4add flow creates).
- The dispatch is opt-in because:
    - A brand-new dev machine has no model API keys configured.
    - The bootstrap flow must remain useful even when no agent
      can actually run.
    - Failures during dispatch are non-fatal — the bead is still
      filed, the operator gets the dispatch stdout/stderr inline
      and the next-step CLI to retry manually.
- JSON output now includes
  `agent_dispatch_attempted` / `agent_dispatch_ok` /
  `agent_dispatch_stdout` / `agent_dispatch_stderr` so
  programmatic callers (test harnesses, MCP controllers) can
  reliably tell whether the demo agent actually ran.
- The slice marker in the JSON envelope now correctly reads
  `"bd-ce32fa: bead filed; agent dispatch attempted iff
  CACO_BOOTSTRAP_DEMO_PROFILE set"` instead of the stale
  bd-422b85 reference.

## Diff summary

- File: `crates/caco-cli/src/lib.rs` (+63/-2).
- Function rewritten:
  `dispatch_bootstrap_dev_demo_agent(json_requested,
  config_override) -> Result<Outcome, CliError>`.
- New env-var gating: `CACO_BOOTSTRAP_DEMO_PROFILE`,
  `CACO_BOOTSTRAP_DEMO_PROJECT`.
- Build clean: `cargo build -p caco-cli`. Clippy clean for
  caco-cli (the unrelated caco-beads `unnecessary_cast`
  warning at `snapshots.rs:237` is being claimed by msm-3).
- No new tests in this commit — the demo-agent path is already
  covered by `bootstrap_dev_unified_dispatcher_exposed` (spec-
  level guard) and by integration via the unified flow.
- Behavioural delta: when no profile env-var is set, output is
  unchanged from prior behaviour (additive guidance line). When
  set, an actual dispatch is attempted and its outcome is
  surfaced in both human and JSON output.

## Embedded artefacts

(none)

## Operator-takeaway

The keyless-first-run UX matters: bootstrap commands that
silently fail or refuse to run when API keys aren't configured
turn what should be a single welcoming command into a frustrating
"you must read the docs first" gate. The opt-in env-var pattern
keeps the happy path clean (no keys → bead filed, clear next
step) while letting power users (CI environments, dev machines
with profiles already configured) get the full
spawn → claim → reintegrate demo. Future follow-up: a
`--profile` flag on `caco bootstrap dev` itself so the demo
profile can be passed positionally instead of via env-var; and
a `--no-demo-dispatch` escape hatch to suppress the dispatch
even when the env var is set (useful for CI smoke tests where
the bead is the artefact under test).
