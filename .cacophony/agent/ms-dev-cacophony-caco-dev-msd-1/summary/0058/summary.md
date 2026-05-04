# bd-bf58e7 — persistent-specialist profile guard docs

## Bead
- bd-bf58e7 — Document persistent-specialist profile guard

## Changes
- Added the generated shipped-profile row for `persistent-specialist` in `docs/profiles.html`.
- Expanded the Agent Profiles page guidance for when to use:
  - `persistent.yaml`: endless dev workers that intentionally keep generic worker lifecycle behavior.
  - `persistent-specialist.yaml`: long-lived specialist implementers that need the shared worker/base stack while the concrete role profile owns scoped work selection and suppresses queue-draining / `/bead-claim` pool behavior.
  - `persistent-observer.yaml`: non-implementing observer/controller roles that must avoid worker-only bead completion guidance.
- Updated `README.md` and `AGENTS.md` so top-level operator/contributor guidance matches the profile docs.

## Validation
- `docs/validate-pages.sh` — passed, 3291 checks.
- `git diff --check` — passed.
- `caco test run --wait --command "cargo test -p caco-profile --lib shipped_profiles_html_matches_autogen_output"` — passed as job `tj-f52c38fa` with 1 test run.
- A prior overly-specific queued command with `-- --exact` passed but ran 0 tests (job `tj-ce40d640`); the follow-up job above is the meaningful profile-docs drift validation.

## Notes
- No raw cargo was run locally for validation; the profile-docs drift check used the first-party test queue.
- Already-running persistent agents that need the updated guard/profile guidance still need their profile artifacts refreshed or the agent recreated.
