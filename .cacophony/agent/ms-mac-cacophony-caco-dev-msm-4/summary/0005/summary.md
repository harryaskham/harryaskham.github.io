# Session summary — caco tendril compatibility guidance

## Goal

Start the operator-assigned TUI Ghostty/Tendril driver loop by first fixing the immediately observed first-party computer-control confusion: `caco tendril` and `caco tendril --help` should not leave operators with `no command supplied` or `unknown command path` when the supported control surface is the separate Tendril CLI / Pi MCP tools.

## Bead(s)

- `bd-c1c272` — [PERMANENT] TUI ghostty/tendril improvement and computer-control audit
- `bd-0bdb2a` — caco tendril command surface is missing despite first-party computer-control expectation

## Before state

- Failing commands: `caco tendril` returned `error: no command supplied`; `caco tendril --help` returned `error: unknown command path for help: caco tendril`.
- Relevant metrics: included one downsized manual preview screenshot for traceability; future captures should stay low-quality / narrow-region by default because screenshots fill context quickly.
- Context: the codebase already documents that Tendril itself is a separate project/CLI, but the Cacophony CLI did not offer actionable compatibility guidance at the command path the operator naturally tried.

## After state

- Failing tests: none in the targeted validation run.
- Relevant metrics: `caco tendril --help` now renders actionable compatibility guidance, including direct `tendril list`, `tendril capture`, `tendril run`, and Pi MCP tool names; bare `caco tendril` now explains it is compatibility help rather than an action surface.
- Context: this keeps the current architectural boundary honest while removing the dead-end help path encountered during TUI driver setup.

## Diff summary

- Commits: `592b8214b`
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: added focused unit coverage for registered Tendril compatibility help and for bare `caco tendril` no longer reporting `no command supplied`.
- Behavioural delta: Cacophony now points operators and agents to the supported Tendril control surfaces instead of failing discovery.

## Embedded artefacts

- `screenshots/manual-border-preview.png` — downsized operator-provided Ghostty preview showing the kind of border drawing artefacts the follow-on TUI audit loop should investigate.

## Operator-takeaway

The TUI driver loop now has a cleaner first step: if an operator or agent reaches for `caco tendril`, the CLI explains the real Tendril routes instead of implying a broken or missing Cacophony action wrapper.
