# bd-e73f7a Recorded PR-backed artefacts

## Goal
Implement recorded PR-backed reintegration support so PR code branches do not carry `.cacophony/agent/...` summary artefacts, while summaries are still durably published to the cacophony-state branch and linked from generated PR bodies.

## Bead(s)
- bd-e73f7a — `[recorded] Support PR-backed recorded reintegration via state artifacts`

## Before state
Direct-branch PR reintegration could push agent branch content and open/update a PR, but recorded summaries were not guaranteed to be split into cacophony-state for PR-backed flows. A PR branch could carry `.cacophony/agent/...` artefacts, and the generated PR body did not identify the state branch commit/path that held the summary.

## After state
Recorded direct-branch PR flow now publishes `.cacophony/agent/...` artefacts to the configured state branch before PR work proceeds, refuses to continue if no durable recorded artefact is produced, uses a code-only publish ref for the PR branch when recorded artefacts are present, and adds a generated Cacophony summary section to PR bodies with the state branch, artefact commit, and paths.

## Diff summary
- Updated `crates/caco-daemon/src/reintegration.rs` direct-branch flow to split recorded artefacts into cacophony-state and push them before PR creation/update.
- Added code-only publish ref synthesis so recorded artefacts are removed from the PR branch without mutating the agent checkout history.
- Extended generated PR body content with a Cacophony recorded artefacts section.
- Added focused daemon tests for state publication and PR body links.

## Validation
- `timeout 900 cargo test -p caco-daemon --lib 'bd_e73f7a' -- --nocapture`
- `timeout 900 cargo test -p caco-daemon --lib direct_branch -- --nocapture`
- `timeout 900 cargo test -p caco-cli --lib 'bd_e5e1bd' -- --nocapture`
- `timeout 600 cargo check -p caco-daemon -p caco-cli`
- `timeout 1500 cargo test-small`

## Operator-takeaway
bd-e73f7a is implementation-complete and validated without local Docker. PR-backed recorded reintegration now keeps code branches clean while preserving summary artefacts on cacophony-state and linking them from the PR body.
