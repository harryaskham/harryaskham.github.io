# Session summary — bd-82d3f4 dev speak cadence

## Goal

Implement Harry's request that dev-oriented profiles speak regular progress updates around every major action, while preserving anti-spam guidance and relying on first-party `caco msg speak` rather than unmanaged loops.

## Bead(s)

- `bd-82d3f4` — Ensure dev profiles speak regular progress updates

## Before state

- Existing `speak`, `dev`, and `worker` profile text asked agents to narrate when something materially changed, but did not explicitly spell out Harry's requested "every major action" cadence.
- `SPEC.md` described the post-tool-use speak heartbeat as a floor, but the voluntary narration expectation was weaker and less concrete.
- README's worker-profile section documented lifecycle/profile behavior but did not mention the major-action narration contract.

## After state

- `.cacophony/profiles/speak.md` now says dev/worker profiles should speak every major action or phase change and lists the concrete events: claim/start, complex/risky edits, meaningful steps, queued validation/build start and finish, blockers/decisions, and reintegration/completion results.
- `.cacophony/profiles/dev.md` and `.cacophony/profiles/worker.md` now include the same major-action cadence in their coordination sections while explicitly avoiding per-file-edit spam.
- `SPEC.md`, `README.md`, and generated profile instruction text under `crates/caco-profile/src/*.txt` now align with the checked-in profiles.
- Lightweight validation passed: `git diff --check` and a Python text-presence check over all touched files.

## Diff summary

- Code/content commits: `3884f1bb2` (`bd-82d3f4: require dev speak at major actions`)
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `.cacophony/profiles/speak.md`, `.cacophony/profiles/dev.md`, `.cacophony/profiles/worker.md`, `SPEC.md`, `README.md`, `crates/caco-profile/src/common_instructions.txt`, `crates/caco-profile/src/agent_instructions.txt`, `crates/caco-profile/src/bead_worker_instructions.txt`
- Tests: +0 / -0 / flipped 0; documentation/profile text-only change
- Behavioural delta: future materialized dev/worker prompts now require concise speak narration at major actions, with the existing heartbeat remaining a fallback floor rather than a substitute for voluntary narration.

## Operator-takeaway

The speak cadence is now explicit and concrete across source profiles, generated instruction text, SPEC, and README: dev workers should narrate each major action or phase change, but should still avoid noisy per-file-edit updates.
