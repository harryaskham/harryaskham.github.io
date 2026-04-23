# Session summary — bd-6d4856 test isolation lint

## Goal

Prevent the bd-03a276 class of silent flake (parallel tests
colliding on tmux socket caco-agent-<project>-<node>-<key>)
from recurring.

## Bead(s)

- `bd-6d4856` (P3 task) — test isolation lint for hard-coded
  persistent decl keys in test fixtures.

## Before state

- bd-03a276 patched ONE of three persistent-decl fixtures.
- Two other #[tokio::test] fixtures still used literal "ctrl"
  → silent collision under cargo test parallelism (30s handler
  timeout, opaque failure).
- No lint to prevent regression.
- Test build broken on main: TopLevelBeadsConfig missing the
  new peer_consult_timeout_ms field at 4 literal sites.

## After state

- Two remaining fixtures migrated to unique-per-process decl_key
  (std::process::id() + per-test discriminator).
- New meta-test `lint_no_literal_persistent_decl_keys_in_async_tests`
  scans crates/caco-daemon/src/ for the dangerous pattern inside
  any #[tokio::test] body. Verified to trip on a deliberately-bad
  variant; clean on the migrated tree.
- Escape hatch: `// bd-6d4856: lint-allowed` annotation.
- Build unblocked: peer_consult_timeout_ms: None added at all
  four sites.

## Diff summary

- `crates/caco-daemon/src/lib.rs`: +137 / -3 — two fixture
  migrations + lint test.
- `crates/caco-daemon/src/beads.rs`: +2 — broken-on-main fix.
- `crates/caco-daemon/src/election.rs`: +1 — broken-on-main fix.
- `crates/caco-daemon/tests/multinode.rs`: +1 — broken-on-main fix.
- cargo test-small green (2849 tests, +7 net); clippy clean.

## Operator-takeaway

Future contributors who add persistent-agent fixtures via
copy-paste will trip the lint instead of shipping silent flakes.
The bd-cf99b7 alarm-fatigue logic from earlier this session
combined with this lint covers two distinct test-quality
failure modes.
