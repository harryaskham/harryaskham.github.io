# Summary — bd-a85a77 router observer reintegration mode conflict

## Goal

Fix broken-on-main `cargo test-small` failure in `crates/caco-profile/src/lib.rs::tests::router_and_narrator_observer_stacks_avoid_reintegration_mode_conflict`, where router composed with the `persistent-observer` stack but still reported `reintegration.mode: direct` instead of the expected no-reintegration `none` policy.

## Before state

- `.cacophony/config.yaml` and `.cacophony/agents/cacophony_persistent.yaml` already routed router/narrator through `persistent-observer.yaml` instead of `persistent.yaml`.
- `.cacophony/profiles/router.md` still carried legacy bd-37a2ff direct-compatible metadata (`reintegration.mode: direct`, allowed direct, lifecycle reintegrate), which contradicted the no-worker observer stack and failed the caco-profile lib assertion.
- SPEC/README/AGENTS still contained older guidance implying router should remain compatible with the shared `endless` worker lifecycle.

## After state

- Router frontmatter now declares `reintegration.mode: none`, `allowed_modes: [none]`, and non-reintegrating self lifecycle `[discard]`.
- The historical router/endless compatibility test was replaced with a regression test asserting router remains a no-reintegration observer, forbids complete/reintegrate, and intentionally conflicts if composed directly with `endless`.
- SPEC, README, AGENTS, and generated `docs/profiles.html` now document/router-list the no-reintegration observer stack.
- Removed runtime-local previous-summaries AUTOGEN blocks from `AGENTS.md` after the reintegration guard refused to publish tracked session-local prompt content.

## Validation

After stale-branch rejection, refreshed with `caco agent rebase --id $CACO_AGENT_ID` onto main `22af053ee` (after an intermediate `c311dc9fb` rebase) and re-ran the release-blocking/required checks.

Passed:

- `cargo test -p caco-profile router_and_narrator_observer_stacks_avoid_reintegration_mode_conflict -- --nocapture`
- `cargo test -p caco-profile router_profile_stays_no_reintegration_observer_bd_a85a77 -- --nocapture`
- `cargo fmt --all -- --check`
- `cargo test-small`
- `cargo build`
- `cargo clippy --workspace --all-targets -- -D warnings`
- `caco config validate --config .cacophony/config.yaml` (valid with existing warnings)
- `git diff --check`
- post-rebase reruns: targeted router/narrator tests, `cargo test-small`, `cargo build`, `cargo clippy --workspace --all-targets -- -D warnings`, and `git diff --check`
- second post-rebase rerun after main advanced to `22af053ee`: targeted router/narrator tests and `cargo test-small`

Failed due unrelated/pre-existing environment or mainline failures:

- `cargo test -p caco-profile` full crate validation passed lib tests but hit three unrelated `tests/profile.rs` failures. Exact searches found no bead, but creating a tracker was rejected as duplicate of in-progress `bd-571e1a`; evidence was sent to that bead's assignee (`pocket4-cacophony-caco-dev-po4-3`).
- `cargo test` full workspace validation compiled successfully but failed in `crates/caco/tests/acceptance_agent.rs` (14/17 failing around missing agent id/checkout_path/tmux_session). Filed follow-up `bd-baf935` after search found only older closed acceptance-agent failures.

## Changed files

- `.cacophony/profiles/router.md`
- `crates/caco-profile/src/lib.rs`
- `SPEC.md`
- `README.md`
- `AGENTS.md`
- `docs/profiles.html`
