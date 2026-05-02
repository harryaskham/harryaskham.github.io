# Session summary — bd-bb7226 quiet controller health checks

## Goal

Land the smallest contained fix for the project-controller speech-policy bead by making the checked-in runtime wrapper match the already-updated project-controller profile, and by pinning the intended behavior in tests so healthy routine checks stay channel-local while audible speech is reserved for notable events.

## Bead(s)

- `bd-bb7226` — Tune project-controller routine health speech policy

## Before state

- Failing tests: `cargo test -p caco-profile canonical_project_controller_plugin_agent_matches_generated_runtime_agent` was red on current main.
- Relevant metrics: `.cacophony/profiles/project-controller.md` already contained the desired quiet-health policy, but `plugins/caco-agent/agents/project-controller.md` still reflected the older louder idle-health wording, so the checked-in wrapper had drifted away from the canonical profile source.
- Context: this bead did not need a fresh policy design; it needed the shipped runtime wrapper and regression coverage brought back into alignment with the canonical profile text.

## After state

- Failing tests: none in the focused caco-profile validation lane.
- Relevant metrics: the checked-in `plugins/caco-agent/agents/project-controller.md` wrapper now matches the canonical profile again, and a new targeted test explicitly asserts that routine healthy checks remain channel-local while `caco msg speak` is reserved for notable events or explicit audible-update requests.
- Context: the project-controller role now has the intended quiet healthy-check behavior both in source profile text and in the shipped runtime-native wrapper that persistent controller launches consume.

## Diff summary

- Commits: `df9f52e96`
- Files touched: `plugins/caco-agent/agents/project-controller.md`, `crates/caco-profile/tests/profile.rs`
- Tests: `cargo test -p caco-profile project_controller_profile_keeps_routine_health_channel_local_bd_bb7226 -- --nocapture`; `cargo test -p caco-profile canonical_project_controller_plugin_agent_matches_generated_runtime_agent -- --nocapture`; `cargo test -p caco-profile project_controller_profile_prompt_requires_idle_reporting -- --nocapture`
- Behavioural delta: healthy routine project-controller checks are now explicitly pinned as terse in-channel updates, while audible `caco msg speak` is constrained to notable events, worsening anomalies, operator-attention moments, or explicit audible-update requests.

## Operator-takeaway

This was wrapper drift, not a policy debate: the quiet-health controller guidance was already present in the canonical profile, but the shipped project-controller wrapper had not been regenerated. That mismatch is now fixed and covered by a direct regression test.
