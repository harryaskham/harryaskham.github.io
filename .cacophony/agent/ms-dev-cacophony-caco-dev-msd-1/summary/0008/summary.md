# Session summary — bd-06d736 caco test run safety gate

## Goal

Stop bare `caco test run` from silently enqueueing a cargo-test job
inferred from agent context. Make the destructive default require
explicit consent.

## Bead(s)

- `bd-06d736` (P2 bug, test-user) — CRITICAL UX: no
  --yes/--confirm/--dry-run gate; typo or accidental invocation
  spam-queues cargo test runs.

## Before state

- `caco test run` with no flags → silent enqueue against current
  agent's checkout using project default command.
- bd-b76723 unknown-flag warning fires AFTER dispatch (too late).
- 0 tests covering the no-args path.

## After state

- `caco test run` with no flags → exit 2 with structured error
  naming all three opt-in flags (--command, --auto, --print) and
  citing bd-06d736.
- `--auto` preserves the original convenience as an explicit
  opt-in.
- `--print` previews the resolved payload (project, cwd, command,
  timeout) without contacting the daemon. Works in both text and
  --json modes.
- 4 new tests pin the contract: surface (args), bare-text-block,
  bare-json-block, with the bare-block tests calling
  dispatch_test_run directly so the gate is enforced before any
  daemon contact.

## Diff summary

- `crates/caco-cli/src/lib.rs`: +151 / 0 — two new ArgSpecs, gate
  + --print branch in dispatch_test_run, four tests.
- Behavioural delta: production behaviour now blocks the silent
  enqueue path. All three opt-in flags are documented in --help.
- cargo test-small workspace-wide green (2837+); clippy clean.

## Operator-takeaway

The convenience path is preserved (--auto) and even improved
(--print is a new capability). The dangerous default is gone. The
error message is self-documenting so operators discovering this
mid-script get exactly the migration path they need without
consulting --help.
