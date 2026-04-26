# Session summary — Persistent web and Android UX loops

## Goal

Ensure Harry's requested permanent caco-web and caco-android UX/beauty tracks are represented as real endless persistent Cacophony declarations on ms-mac, and clean up the profile-validation drift discovered while proving the config.

## Bead(s)

- `bd-9653c3` — [persistent-loops] Ensure caco-web and caco-android profiles run as ENDLESS dev loops on ms-mac
- `bd-1d4a60` — [broken-on-main] caco-profile shipped profile validation drift

## Before state

- Failing tests: `cargo test -p caco-profile --test profile` exposed shipped profile drift: `pi_self_nudge_profile_is_pi_only`, `pi_self_ops_profile_is_pi_only`, generated plugin wrapper comparisons, and stale unknown-MCP expectation. `docs/profiles.html` also drifted after the caco-web profile metadata changed.
- Relevant metrics: `caco agent list` showed no live `caco-web` or `caco-android` specialist persistent agents on ms-mac. The derived config did not contain `caco-web`/`caco-android` project persistent declarations.
- Context: multiple peers saw the same docs/profile drift during validation; ownership was coordinated in project chat so the fix lands once with this stream.

## After state

- Failing tests: none observed in the targeted validation for this work.
- Relevant metrics: `caco config validate --strict` passes; derived config from the edited `.cacophony/config.yaml` resolves both `caco-web` and `caco-android` persistent declarations on `ms-mac`; `cargo test -p caco-profile --test profile`, `cargo test -p caco-profile --lib`, `cargo test -p caco-config persistent -- --nocapture`, `docs/validate-pages.sh`, and `just docs-check` pass.
- Context: `caco-web` now has full persistent frontmatter matching `caco-android`/`caco-tui` style, and the checked-in Claude plugin wrappers/docs were regenerated so generated profile checks are stable again.

## Diff summary

- Commits: `e3827e7d6`, `d895052d8`, plus this recorded-summary commit
- Files touched: `.cacophony/agents/cacophony_persistent.yaml`, `.cacophony/profiles/caco-web.md`, `.cacophony/profiles/pi-self-nudge.md`, `.cacophony/profiles/pi-self-ops.md`, `README.md`, `AGENTS.md`, `docs/profiles.html`, `crates/caco-profile/tests/profile.rs`, `plugins/caco-agent/agents/{worker,controller,project-controller}.md`
- Tests: profile and config validation lanes restored; docs profile generation check restored.
- Behavioural delta: the repo config will reconcile caco-web and caco-android as ms-mac endless persistent specialist loops after landing and daemon config sync, and caco-profile's shipped-profile validation no longer fails on stale metadata or generated wrapper drift.

## Operator-takeaway

The missing caco-web permanent loop was a declaration/profile-metadata gap, not just a live-process blip: this change makes web and Android UX loops explicit, persistent, endless, and non-autoclaim in the checked-in project config.
