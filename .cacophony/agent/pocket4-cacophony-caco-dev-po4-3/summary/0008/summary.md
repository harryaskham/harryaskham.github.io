# Session summary — nav test runtime-state isolation

## Goal

Fix `bd-9e020b`, a broken-on-main caco-tui regression where several `app::tests::*nav*` tests panicked because their shared agent-detail fixture could not find the expected `agent-1` sidebar row.

## Bead(s)

- `bd-9e020b` — [broken-on-main] app::tests::enter_nav_resets_agent_detail_tab_for_running_agent and related nav tests panic 'agent row should exist'

## Before state

- Failing tests: `RUST_MIN_STACK=33554432 cargo test -p caco-tui --lib enter_nav_resets_agent_detail_tab_for_running_agent -- --test-threads=1` failed with `agent row should exist`; broader `app::tests::*nav*` checks also failed when the test app inherited the live runtime's last workspace project and rendered the wrong project tree.
- Relevant metrics: `cargo test -p caco-tui --lib nav_` initially had multiple app nav failures, plus one unrelated stale nav_tree sublabel expectation.
- Context: the fixture created a synthetic `cacophony` project, but `App::new` in tests could still restore global runtime state from the operator's real `$CACOPHONY_DIR` when no explicit layout path was supplied.

## After state

- Failing tests: none in the targeted app-nav validation.
- Relevant metrics: passed `RUST_MIN_STACK=33554432 cargo test -p caco-tui --lib enter_nav_resets_agent_detail_tab_for_running_agent -- --test-threads=1`, `RUST_MIN_STACK=33554432 cargo test -p caco-tui --lib app::tests::nav_ -- --test-threads=1`, targeted enter/mouse/rebuild/speech nav filters, `cargo clippy -p caco-tui --lib -- -D warnings`, `./scripts/rustfmt-changed.sh`, and `git diff --check`.
- Context: `cargo test -p caco-tui --lib nav_` still includes `views::nav_tree::tests::persistent_sublabel_renders_lifecycle_text_not_bare_checkmark`, which fails independently against newer persistent-sublabel behavior and is outside this app-nav bead.

## Diff summary

- Code/content commits: `fd1f06302`.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched: `crates/caco-tui/src/app.rs`.
- Tests: updated app test setup and one brittle nav-tree ordering assertion; no new production feature tests added.
- Behavioural delta: test builds of `App::new` no longer fall back to the live runtime global-state file when no layout path is supplied, and the shared agent-detail fixture clears ambient state, selects the intended workspace, expands the agent path, and prefers the node-level agent row before falling back to aggregate rows.

## Operator-takeaway

The broken app nav tests were not indicating a user-facing sidebar regression; they were leaking the operator's live workspace state into unit tests. The fix makes the app-nav tests deterministic again while preserving production runtime-state restore behavior outside `cfg(test)`.
