# Session summary — Fix Pi startup session-name command

## Goal

Correct the managed Pi startup/resume naming prompt after Harry clarified that Pi's runtime command is `/session-name`, not `/name`, so new and resumed managed Pi sessions are named with the stable Cacophony agent ID using the supported slash command.

## Bead(s)

- `bd-e4f598` — Fix Pi startup session naming slash command

## Before state

- Managed Pi fresh launches and resume/continue rewrites injected a leading `/name ${CACO_AGENT_ID}` prompt before the managed task/nudge prompt.
- SPEC, README, and AGENTS documented `/name <agent-id>` as the Pi session naming command.

## After state

- Managed Pi startup/resume helpers now inject `/session-name ${CACO_AGENT_ID}`.
- Strip/idempotency helpers and daemon unit tests expect `/session-name` in fresh, continue, and explicit-session resume flows.
- SPEC, README, and AGENTS now document `/session-name <agent-id>`.

## Diff summary

- Code/content commits: `dd013e44d`
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `crates/caco-daemon/src/agent/spawn.rs`, `crates/caco-daemon/src/agent/tests.rs`, `SPEC.md`, `README.md`, `AGENTS.md`
- Tests: +0 / -0 / flipped existing expectations from `/name` to `/session-name`
- Validation: `git diff --check`; no remaining managed Pi `/name` startup literals in the touched code/docs; queued daemon helper tests passed: `tj-49c09d3b` (`inject_pi_continue_flag`), `tj-61d94347` (`inject_pi_session_flag`), and `tj-2b67e2de` (`init_script_pi_uses_append_system_prompt`). One attempted filter `pi_uses_separate_system_prompt_file_for_pi` matched zero tests and failed queue validation as expected; reran with the correct test name.
- Behavioural delta: Pi session selectors should now receive the supported `/session-name` command before the managed task prompt, preserving the existing safety property that any naming-command warning cannot swallow real instructions.

## Operator-takeaway

The Pi session naming startup hook now uses the correct runtime slash command everywhere Cacophony injects or documents it, so managed Pi sessions should be named by agent ID without relying on an unsupported `/name` alias.
