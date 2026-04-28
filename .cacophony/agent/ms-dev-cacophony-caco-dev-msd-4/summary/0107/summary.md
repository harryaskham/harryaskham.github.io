# Session summary — bounded persistent-profile warning spam

## Goal

Fix the repeated `profile '<name>' does not match any configured profile` warnings that were flooding helsinki restart logs and `daemon-crash.log` whenever config validation ran before profile discovery had converged. The target was not to make genuinely missing profiles fatal, but to keep startup/restart observability usable while preserving structured persistent-agent state for unavailable profiles.

## Bead(s)

- `bd-002eda` — `daemon-crash.log repeatedly warns persistent profiles are missing after restart`

## Before state

- Failing tests: none known for this bead.
- Relevant metrics: helsinki evidence reported 1,911 matching warning lines in a bounded `daemon-crash.log` tail and a restart supervisor excerpt saying the previous daemon stderr had 8,579 lines, with the last 40 dominated by persistent profile warnings.
- Context: Source already deduped missing-profile warnings within a single validation call, but `validate_config*` can run repeatedly during daemon startup/restart convergence. Each call rebuilt the per-call set, so the same missing profile names were emitted again and again before profile discovery caught up.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: The config validator now keeps a process-wide `OnceLock<Mutex<HashSet<String>>>` of missing persistent profile names already warned about. Each profile name emits at most one non-blocking stderr warning per daemon process; per-declaration missing-profile details continue to live in persistent sentinel state and the existing warning collection helper.
- Context: `SPEC.md` now documents the bounded-warning contract for missing persistent profiles during startup/profile-discovery convergence.

## Diff summary

- Commits: `fd4985147` (`bd-002eda: bound persistent profile validation warnings`) plus the summary-only commit for this record.
- Files touched: `crates/caco-config/src/validate.rs`, `SPEC.md`.
- Tests: added `persistent_missing_profile_warning_guard_is_process_wide` and reran existing persistent profile warning tests.
- Behavioural delta: repeated config validation in one daemon process can no longer flood stderr/`daemon-crash.log` with the same missing-profile warning. If the process restarts, the warning can appear once again, which keeps a real current configuration problem visible without high-volume repetition.
- Validation: `cargo fmt --all -- --check`; `cargo test -p caco-config persistent_missing_profile_warning_guard_is_process_wide`; `cargo test -p caco-config warns_persistent_decl_unknown_profile`; `cargo test -p caco-config warns_composite_profile_partial_missing`.

## Operator-takeaway

The noisy log signature was caused by per-call dedupe rather than process-wide dedupe. Missing profiles can still temporarily block persistent agents until discovery catches up, but the daemon should now produce a bounded one-time warning burst instead of thousands of repeated stderr lines during restart convergence.
