# Session summary — managed Pi session naming

## Goal

Implement `bd-06a2a9` so managed Pi sessions are named semantically from the stable Cacophony agent ID before the normal managed task or resume/nudge content is delivered, making Pi session history easier to correlate with Cacophony lifecycle surfaces.

## Bead(s)

- `bd-06a2a9` — Name managed Pi sessions semantically from Cacophony agent IDs

## Before state

- Failing tests: none known at start.
- Relevant metrics: managed Pi launch injected system prompt and goal prompt arguments, and resume helpers rewrote Pi commands with `--continue` / `--session`, but none inserted a `/name <agent-id>` initial message.
- Context: Pi provider session storage already pointed at `~/.pi/agent/sessions/`, but session display names were not automatically tied to Cacophony agent IDs.

## After state

- Failing tests: none in focused queued validation.
- Relevant metrics: fresh Pi launches, fresh Pi relaunches, `pi --continue`, and `pi --session <path>` resume rewrites now include a managed `/name ${CACO_AGENT_ID}` initial message, deduplicated during resume/fork prompt stripping and ordered before the managed goal prompt.
- Context: README, AGENTS, and SPEC now document that managed Pi sessions live in Pi's external session store and are named from the stable Cacophony agent ID before task/nudge prompt delivery.

## Diff summary

- Code/content commits: `4d3d9fd8a`
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `crates/caco-daemon/src/agent/spawn.rs`, `crates/caco-daemon/src/agent/tests.rs`, `SPEC.md`, `README.md`, `AGENTS.md`
- Tests: updated/added focused daemon helper assertions for Pi fresh launch ordering, fresh relaunch, continue, and session resume naming.
- Behavioural delta: managed Pi processes receive `/name ${CACO_AGENT_ID}` as a separate first initial prompt before the task prompt; if Pi treats `/name` as unsupported, the next initial prompt still carries the managed instructions.
- Validation: `./scripts/rustfmt-changed.sh`; `git diff --check`; queued `just caco-daemon-helper-test inject_pi` (job `tj-6c832c4b`); queued `just caco-daemon-helper-test init_script_pi_uses_append_system_prompt` (job `tj-1a2389d3`); queued `just caco-daemon-helper-test build_resume_init_script_pi` (job `tj-2cf8be84`); queued `just caco-daemon-helper-test init_script_pi_does_not_include_continue_on_fresh_launch` (job `tj-2c81c6a0`).

## Operator-takeaway

Managed Pi session history should now be self-identifying: session selectors can show the same stable agent ID that Cacophony lifecycle, board, and status surfaces use, without risking loss of the actual task prompt if the naming command fails.
