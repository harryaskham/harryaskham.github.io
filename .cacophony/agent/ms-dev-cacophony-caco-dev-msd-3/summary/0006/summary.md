# Session summary — bd-aa4add: unified `caco bootstrap dev` onboarding

## Goal

Make first-run developer onboarding a single command. Previously
`caco bootstrap dev` required exactly one of six mutually-exclusive
mode flags (`--check`, `--init-config`, `--start-daemon`,
`--create-project NAME`, `--join PROJECT`, `--demo-agent`); a bare
invocation errored. The bead's vision: a new contributor runs
`caco bootstrap dev` and the tool walks them through the entire
setup sequence, presenting per-step output and stopping with
actionable guidance if anything fails.

## Bead(s)

- `bd-aa4add` — First-run developer onboarding: `caco bootstrap
  dev` single-command sets up checkout, project, daemon,
  persistents, sample bead

## Before state

- All five sub-flows already implemented across earlier slices
  (`bd-334962`, `bd-ce32fa`, `bd-6a50ec`, `bd-422b85`).
- Bare `caco bootstrap dev` returned a `CliError`: "requires one
  of: --check, --init-config, --start-daemon, --create-project
  NAME, --join PROJECT, --demo-agent."
- Spec summary advertised the six flags but no unified flow.
- New developers had to manually read the help, learn the flag
  ordering, and run six separate commands in the right sequence.

## After state

- Bare `caco bootstrap dev` (no flags) now routes to a new
  `dispatch_bootstrap_dev_unified` that runs all five steps in
  order:
    1. `step 1/5: precondition check` — calls
       `dispatch_bootstrap_dev_check`. On failure halts with
       "Fix the issues above and re-run `caco bootstrap dev`."
    2. `step 2/5: init config` — calls
       `dispatch_bootstrap_dev_init_config(false, false, None)`
       (non-force, default node-name).
    3. `step 3/5: start daemon` — idempotent.
    4. `step 4/5: create sandbox project` — creates a "sandbox"
       project (idempotent if it exists).
    5. `step 5/5: file demo bead` — files the synthetic
       hello-world bead so any persistent worker can immediately
       claim it.
- After all steps, prints a "bootstrap complete" footer with
  config path, project name, and three suggested next-step
  commands (`caco bd list`, `caco agent dispatch …`,
  `caco doctor`).
- Honours `--json` (machine-readable transcript with step list).
- All six per-flag modes still work for re-running individual
  steps.
- Spec summary updated: now advertises the bare invocation first
  and references `bd-aa4add`. The per-flag modes are listed as
  individual-step alternatives.

## Diff summary

- File: `crates/caco-cli/src/lib.rs` (+156/-8).
- New function: `dispatch_bootstrap_dev_unified(json_requested,
  config_override) -> Result<Outcome, CliError>` (~100 LoC).
- Spec edit: `BOOTSTRAP_SUBCOMMANDS[0].summary` rewritten to
  advertise the bare invocation.
- Routing edit: the fallthrough `Err` in the bootstrap-dev arm of
  the dispatch table is replaced by a tail-call to the unified
  dispatcher.
- Test: `bootstrap_dev_unified_dispatcher_exposed` — asserts the
  dispatcher symbol is linkable as a function pointer (compile-
  time guard against rename / removal) and the spec summary
  documents the bare invocation and the bd-aa4add reference.
- A heavier integration test (actually invoke the unified
  dispatcher in a tmpdir) was tried but caused a stack overflow
  because the underlying `dispatch_bootstrap_dev_create_project`
  shells out to `current_exe()` — which in test mode is the test
  binary, recursing back into the dispatcher. The lighter spec-
  level test pins the contract without the recursion hazard. A
  follow-up using `assert_cmd` against the built `caco` binary
  would close the integration gap.
- Build clean: `cargo build -p caco-cli`; clippy clean.
- 1/1 new test passes; full bootstrap-related test family
  unaffected.

## Embedded artefacts

(none)

## Operator-takeaway

The five mode flags were a nice low-risk way to ship the
underlying state-mutating logic across many small slices, but
their existence as the only entry-points was a UX bug — they
forced new developers to learn the implementation decomposition
before they could complete onboarding. The unified flow keeps the
slices intact (they are still individually invocable for
recovery) but makes the bare command the obvious default. Future
follow-ups: (a) parameterise the project name (currently hard-
coded "sandbox"); (b) replace the recursion-prone `current_exe()`
shell-outs in the underlying dispatchers with direct in-process
calls so the integration test can be re-enabled; (c) consider an
interactive prompt mode (`caco bootstrap dev --interactive`)
that confirms each step before running.
