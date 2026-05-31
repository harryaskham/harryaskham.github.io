# Session summary — caco-config test-small green-up (agent_defaults classification)

## Goal

Operator asked the fleet to focus on project health from a software-engineering perspective. As an idle generic worker with little non-specialist queue work, I ran an authoritative `cargo test-small` health check on main, found it RED on three caco-config contract tests (a gap the recent caco-tui green-up bd-8b30dc did not cover), triaged each stale-vs-real, and fixed the one unambiguous stale-classification failure while routing the two operator-facing config-contract conflicts to the config owner instead of flipping them blindly.

## Bead(s)

- `bd-1188a1` — [broken-on-main] caco-config alignment test: override_pi_agent_dir agent_defaults key unclassified (this session)
- `bd-4502e3` — [broken-on-main] caco-config contract drift: default theme high-vs-enterprise + sgu24 termux-tts (filed for config-owner decision; not implemented here)
- related: `bd-8b30dc` — caco-tui test-small green-up (closed; did not cover caco-config)

## Before state

- `cargo test-small` RED on main (queued tj-eb00239a): caco-config `--lib` 887 passed / 3 failed.
- Failures: `profile_agent_defaults_alignment_decisions_are_exhaustive_bd_a98020` (model.rs), `enterprise_theme_is_registered_and_well_formed` (lib.rs), `checked_in_config_exposes_termux_tts_profile_for_sgu24_bd_a8bd72` (lib.rs).
- `override_pi_agent_dir` (added via 9cde26051c with cascade + profile-frontmatter support) was in `AGENT_DEFAULTS_KEYS` but unclassified in the alignment-decision test.

## After state

- `override_pi_agent_dir` classified as shared-with-profile in `SHARED_PROFILE_AGENT_DEFAULTS_KEYS`.
- Focused queued validation (tj after fix): `profile_agent_defaults_alignment_decisions_are_exhaustive_bd_a98020 ... ok`; caco-config crate compiles clean.
- Remaining 2 caco-config failures are operator-facing config-vs-contract drift documented in bd-4502e3 for the config owner; not flipped unilaterally.

## Diff summary

- Code/content commit(s): pending final squash SHA from reintegration receipt.
- Files touched: `crates/caco-config/src/model.rs` (test classification list only).
- Tests: +0 / -0 / 1 flipped red->green (no production-behavior change).
- Behavioural delta: none in production; only the alignment contract test now recognizes the existing `override_pi_agent_dir` shared key.

## Operator-takeaway

test-small is still RED on main on two operator-facing caco-config contracts that config-helper changed intentionally: the default TUI theme is now `high` (test still expects `enterprise`, bd-b45b04) and sgu24's event-facing speech config no longer exposes the `termux-tts` profile (test bd-a8bd72; config-helper moved the profile registry to the PID-only daemon service on purpose). Both need a config-owner/operator decision (config vs contract test) — captured in bd-4502e3 — before test-small can go fully green.
