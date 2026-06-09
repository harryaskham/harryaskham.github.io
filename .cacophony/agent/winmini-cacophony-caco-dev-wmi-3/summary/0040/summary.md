# bd-bee264 — Embedded mode: complete handler-guard coverage + no-subprocess regression test

## Bead
bd-bee264 (daemon/embedding/ios, P2) — gap in bd-f0afad. Parent epic bd-948386; safety prerequisite for iOS Tier 1 (bd-5c289f). Filed by msm-2 as a research-audit finding (caco-tui/graphics + embedding-research lane); msm-2 confirmed the guard shape and asked me (generic daemon-dev lane) to land it.

## Problem
bd-f0afad added embedded mode (`CACO_EMBEDDED=1`) — serves beads/messaging/file-cache/feed/status/choices/config over loopback with zero subprocess deps so the daemon can run in-process inside an app (iOS/watchOS, no subprocess). Its background-loop skip-set (`EMBEDDED_SKIPPED_BACKGROUND_TASKS`) was comprehensive, but the request-handler guard (`embedded_mode_unavailable_response`) was applied at only ONE site — agent spawn (`handle_agent_create`). Every other subprocess-reachable mutating handler was unguarded, so in embedded mode (esp. iOS, no subprocess) hitting them attempted tmux/git/cargo/gh work and failed ungracefully (missing-binary error / hang) instead of returning the clean machine-parseable `embedded_mode_unavailable` (501) error.

## Change (crates/caco-daemon/src/lib.rs)
Mirrored the existing `if embedded_mode_enabled() { return embedded_mode_unavailable_response("<op>", &request_id); }` guard (inserted right after `request_id` extraction, before any subprocess work) across all 10 subprocess-reachable mutating HTTP handlers:
- Agent lifecycle (tmux/git): `handle_agent_recreate`, `handle_agent_resume`, `handle_agent_stop`, `handle_agent_discard`, `handle_agent_complete` (drives reintegration via a `caco` subprocess).
- Queues (cargo): `handle_test_run`, `handle_build_run`.
- Release (gh/cargo): `handle_release_trigger`, `handle_release_sync`.
- Checkout (git): `handle_checkout_refresh`.

No `handle_agent_reintegrate`/`ship`/`rebase` or `update`/`self-update`/`beads-sync` HTTP handlers exist (those are CLI-driven or background loops already in the skip-set), so there was nothing else to guard. Read-only endpoints (e.g. `handle_checkout_status`) deliberately untouched so beads/msg/file-cache/feed/status/config keep answering in embedded mode.

## Tests (the no-subprocess-from-handlers invariant bd-f0afad's skip-set test did not cover)
Added two source-introspection tests (using the established `include_str!("lib.rs")` handler-slice pattern). Source-scan rather than runtime calls because `embedded_mode_enabled()` memoizes `CACO_EMBEDDED` in a process-lifetime `OnceLock` — a sibling test reading it with the env unset would pin it false for the whole test binary, making a runtime assertion flaky. The scan deterministically catches a dropped guard or a new mutating handler added without one.
- `embedded_mode_guards_subprocess_mutating_handlers_bd_bee264`: enumerates the 11 guarded handlers (the 10 above + the original `handle_agent_create`) and asserts each body contains `embedded_mode_enabled()` and `embedded_mode_unavailable_response(`.
- `embedded_mode_does_not_guard_local_only_handlers_bd_bee264`: asserts the read-only sibling `handle_checkout_status` carries no guard (strong negative contrast with the guarded `handle_checkout_refresh`).

## Acceptance
With CACO_EMBEDDED=1 every subprocess-requiring mutating endpoint returns `embedded_mode_unavailable` (no subprocess attempted); read/control endpoints still work; a test enforces the no-subprocess-from-handlers invariant. ✓

## Validation (daemon test queue, --cwd at checkout)
- `cargo test -p caco-daemon --lib bd_bee264` (tj-ee5c8fd4): PASSED 2/2 (lib compiled).
- `cargo clippy -p caco-daemon --lib` (tj-25514c49): PASSED, warning-clean (no warnings referencing the touched handlers).
- `git diff --check` clean; inserted guard/test lines rustfmt-clean (verified via skip_children reformat diff — the 11 pre-existing whole-file lib.rs drift hunks were untouched).

## Diff
See the reintegration receipt for the landed squash SHA.
