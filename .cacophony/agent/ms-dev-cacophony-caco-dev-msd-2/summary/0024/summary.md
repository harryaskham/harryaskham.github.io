# Session summary — project-controller queue-drain guard

## Goal

Prevent project-controller prompts from reading like worker prompts when composed with endless/self-ops style helper mixins, so controllers keep monitoring/routing instead of accidentally draining implementation beads.

## Bead(s)

- `bd-5f0044` — Profile audit: endless profile tells project controller to claim next beads

## Before state

- Failing tests: none; this was a profile-audit wording/guardrail issue.
- Relevant metrics: project-controller profile already had a lifecycle override section, but its control-surface list still grouped `caco bd claim` with routine bead management.
- Context: a reified controller prompt from the source incident mixed worker-style next-task language with project-controller non-worker instructions, creating ambiguity about whether idle controllers should claim implementation work.

## After state

- Failing tests: none in targeted validation; one initial validation command failed because cargo accepts only one test-name filter, then the corrected queued command passed.
- Relevant metrics: `cargo test -p caco-profile project_controller_profile --test profile -- --test-threads=2` passed before rebase as `tj-787691ec`, after first rebase as `tj-46b74f7a`, and after the stale-rejection rebase as `tj-0ed00765`.
- Context: project-controller source and checked-in runtime-native plugin now say `caco bd claim` is only for explicit operator/controller handoffs assigning controller-owned profile/config maintenance, never idle queue-drain.

## Diff summary

- Commits: bead-aware code/profile commit on this agent branch plus this summary artefact commit.
- Files touched: `.cacophony/profiles/project-controller.md`, `plugins/caco-agent/agents/project-controller.md`, `crates/caco-profile/tests/profile.rs`
- Tests: +1 regression test; no tests removed. `tj-1502d3f4` failed due invalid test command syntax, then `tj-787691ec`, `tj-46b74f7a`, and post-stale-rebase `tj-0ed00765` passed.
- Behavioural delta: project-controller materialization now preserves explicit role precedence over endless/auto-claim/self-ops worker helpers, and tests assert the runtime-native Claude agent retains that guardrail.

## Operator-takeaway

The confusing project-controller queue-drain wording is now guarded both in profile text and in bridge/materialization tests, reducing the risk that observer/controller agents silently convert into implementation workers.
