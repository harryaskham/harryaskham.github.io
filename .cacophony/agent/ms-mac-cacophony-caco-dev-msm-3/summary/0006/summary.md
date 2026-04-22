# Session summary — bd-4896e6 caco claude/codex/pi never fall back to default_project

## Goal

Stop `caco claude` / `caco codex` / `caco pi` from silently inheriting `default_project` (or the sole-configured-project shortcut) and instead require an explicit project source — so a missing `--project` fails fast in 0 ms instead of after a 30 s daemon call.

## Bead(s)

- `bd-4896e6` — claude_requires_project_flag check doesn't block config-derived default_project — 30s daemon call when it should fail fast.

## Before state

- `resolve_interactive_project` cascade: flag → env → `interactive_defaults.<runtime>.project` → `default_project` → sole-configured project.
- The last two legs meant `caco claude` could silently target the operator's `default_project` and then make a 30 s daemon call before failing for unrelated reasons. Hides genuine config-mismatch bugs and degrades operator UX.
- Existing test `claude_requires_project_flag` had to scrub `CACOPHONY_DIR` to a fresh empty tmpdir to even test the failure path.

## After state

- `resolve_interactive_project` cascade trimmed to: flag → env → `interactive_defaults.<runtime>.project`. Interactive runtime spawns intentionally do NOT inherit `default_project` or the sole-project shortcut.
- Error message now explicitly cites `bd-4896e6` so future readers know the trim was deliberate.
- New regression test `claude_does_not_fall_back_to_default_project` builds a `CACOPHONY_DIR` with `default_project: only-proj` + a single project and asserts the command STILL fails with the `--project required` error.
- `claude_requires_project_flag` and `pi_requires_project_flag` still pass unchanged.

## Diff summary

- Commits: `9ff6f5e5`.
- Files touched: `crates/caco-cli/src/lib.rs`.
- Tests: 14 `claude_*` tests pass (was 13, +1 new). `cargo clippy -p caco-cli --all-targets -- -D warnings` clean.
- Folded in: bd-ae8de9 trivial fix (added missing `artefact_commit: None` field on two `ReintegrationOutcome` literals in `reintegration_conflict_formatter` tests; the field was added by my prior bd-f76c81 commit in this session and broke the caco-cli build). Coordinated with wmi-2 in #cacophony.
- Behavioural delta:
  - Loses cascade: `caco claude/codex/pi` no longer auto-pick `default_project` or sole-project. Operators relying on this must add `interactive_defaults.<runtime>.project` to their config or pass `--project` / `CACOPHONY_PROJECT`.
  - Gains: missing-project errors now appear in 0 ms instead of after a 30 s daemon call.

## Operator-takeaway

If anyone's muscle-memory was `caco claude` (no flag) and assuming it would pick the right project, they'll see a hard error after this lands and need to either set `interactive_defaults.claude.project: <name>` (per-runtime opt-in) or include `--project` going forward. That's the explicit point of the change — making the project source visible.
