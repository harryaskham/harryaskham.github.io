# Session summary — Pi sudo runner mixin

## Goal

Add a Pi-native privileged-command path for managed agents so operator-approved sessions can run non-interactive sudo automation through a configured wrapper such as `sops-sudo-ni --`, without dropping into an interactive expect flow or exposing sudo password material.

## Bead(s)

- `bd-f62444` — Make sops-sudo-ni a Pi-native sudo runner

## Before state

- Failing tests: none known for this bead.
- Relevant metrics: managed Pi overlays included inbox, loop, self-compact, self-nudge, self-ops, Tendril, tmux-cli, and image-guard helpers, but no first-party privileged-command wrapper.
- Context: high-risk Bash hooks blocked raw `sudo`, while the existing `sops-sudo` workflow was interactive/TTY-oriented and awkward for Pi tool calls that need non-interactive privilege escalation.

## After state

- Failing tests: none observed in focused validation.
- Relevant metrics: final-tree `node --test .cacophony/pi/sudo-runner/extensions/caco-sudo-runner.test.mjs` passed 5 tests; `cargo fmt --all -- --check` passed; queued `tj-838ecc2a` passed `cargo test -p caco-profile pi_sudo_runner -- --test-threads=2` with 3 focused profile tests.
- Context: managed Pi profiles can now compose `pi-sudo-runner`, which materializes `.cacophony/pi/sudo-runner/` and exposes a disabled-by-default `sudo_bash` tool plus `/sudo-runner status|enable|disable` session command.

## Diff summary

- Commits: `e132390ce` plus the summary commit containing this file.
- Files touched: `.cacophony/pi/sudo-runner/extensions/caco-sudo-runner.mjs`, `.cacophony/pi/sudo-runner/extensions/caco-sudo-runner.test.mjs`, `.cacophony/profiles/pi-sudo-runner.md`, `.cacophony/agents/pi-common.yaml`, `.cacophony/project.yaml`, `crates/caco-profile/tests/profile.rs`, `SPEC.md`, `README.md`, `AGENTS.md`.
- Tests: +5 Node extension unit tests; +3 caco-profile regression tests.
- Behavioural delta: Pi workspaces gain a configurable privileged-command helper that defaults internally to `sudo -n --`, can be pointed at `sops-sudo-ni --`, refuses to run unless env/session policy opts in, checks runner availability before execution, keeps the requested command visible for auditability, and never accepts password material in tool inputs.

## Operator-takeaway

This makes `sops-sudo-ni` usable from Pi as an explicit, auditable non-interactive sudo runner while preserving fail-closed defaults. Host config can provide the real runner and opt-in policy without the profile overwriting those environment values.
