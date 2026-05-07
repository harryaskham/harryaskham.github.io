# Session summary — controller lifecycle wording audit

## Goal

Clarify the profile instruction conflict in `bd-6d228e`: generic Cacophony guidance and plugin wrappers made `caco agent complete` look like a normal landing path, while controller/project-controller profiles later forbid it. The goal was to make the role-specific lifecycle rule visible early and make generic command listings explicitly one-shot-worker-scoped.

## Bead(s)

- `bd-6d228e` — profile-audit: agent-complete vs agent-reintegrate guidance is contradictory across base + project-controller profiles

## Before state

- Failing tests: none known before editing.
- Relevant metrics: `bd-6d228e` was assigned to `ms-mac-cacophony-caco-profile` and described a controller prompt conflict from reified instructions.
- Context: controller profiles already forbade `caco agent complete` in their Completion sections, but the shared base instructions and generated plugin wrappers still described `complete` generically as “Finalize your run and reintegrate work.”

## After state

- Failing tests: none remaining in the targeted validation.
- Relevant metrics: `git diff --check origin/main..HEAD` passes; direct foreground `cargo test -p caco-profile plugin_agent_matches_generated_runtime_agent` passes (6 tests).
- Context: shared instruction text now says lifecycle commands are role-sensitive, the `complete` command table describes one-shot/completion-permitted use only, and controller/project-controller profiles state their reintegrate-only rule immediately after the role summary.

## Diff summary

- Commits: lifecycle wording and wrapper regeneration commits for `bd-6d228e`, plus summary/state commits (commit hashes may change across mandatory pre-reintegration rebases).
- Files touched: `.cacophony/profiles/controller.md`, `.cacophony/profiles/project-controller.md`, `crates/caco-profile/src/common_instructions.txt`, `crates/caco-profile/src/agent_instructions.txt`, and regenerated `plugins/caco-agent/agents/*.md` wrappers.
- Tests: +0 / -0 / flipped 0; validation regenerated checked-in wrappers and ran the caco-profile plugin-agent generated-alignment test filter successfully.
- Behavioural delta: controllers reading from the top of their prompt now see that `complete` is forbidden before any operational guidance, and generic plugin/base command tables no longer imply `complete` is universally valid.

## Operator-takeaway

This is a prompt-safety cleanup rather than a runtime behavior change: controller agents should now be less likely to confuse one-shot worker completion with persistent/controller reintegration during incident recovery.
