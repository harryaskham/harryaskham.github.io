# Session summary — align agent rebase tracking refs

## Goal

Fix `bd-f77a97` so `caco agent rebase` does not leave managed worker checkouts visually ahead of stale `origin/main` after rebasing against the authoritative upstream target. The goal was a bounded control-plane change suitable for the high-load sweep: improve the first-party rebase surface without running raw Cargo locally on ms-mac.

## Bead(s)

- `bd-f77a97` — Align agent rebase with local origin/main tracking refs

## Before state

- Failing tests: none reported for this bead.
- Relevant metrics: the reported friction came from `caco agent rebase --id ...` correctly fetching/rebasing against the canonical upstream target while leaving the worker checkout's local `origin/main` tracking ref stale. Post-rebase commands such as `git status` or `git diff --stat origin/main...HEAD` could therefore show many already-landed commits as if they were still local work.
- Context: existing bd-574564 coverage verified the internal authoritative rebase ref, but did not assert that the visible local `origin/main` tracking ref was aligned afterward.

## After state

- Failing tests: none observed.
- Relevant metrics: queued focused validation passed as `tj-d98d0def` for `CC=/nix/store/s7qlr26bmc6n4r607scz8iiwcg6yg4ic-clang-wrapper-21.1.8/bin/clang cargo test -p caco-cli agent_rebase_resolves_local_daemon_origin_to_authoritative_upstream_bd_574564`; `rustfmt --edition 2021 crates/caco-cli/src/lib.rs` and `git diff --check` passed.
- Context: `AgentRebaseTarget` now carries a visible tracking ref and fetches the authoritative target into both the internal rebase ref and `refs/remotes/origin/<target>` when the worker origin is a local daemon checkout. The text and JSON rebase success output identify the aligned tracking ref.

## Diff summary

- Commits: pending commit for `bd-f77a97`
- Files touched: `crates/caco-cli/src/lib.rs`, `SPEC.md`, `README.md`, `AGENTS.md`, this summary
- Tests: updated the existing bd-574564 caco-cli regression test to assert stale `origin/main` is advanced to the upstream tip after the first-party fetch path.
- Behavioural delta: `caco agent rebase` still rebases against the same authoritative internal ref, but also updates the local remote-tracking target ref used by ordinary worker git commands. Documentation now states that this tracking-ref alignment is part of the first-party rebase contract.

## Operator-takeaway

After this change, workers who use the canonical `caco agent rebase` recovery path should see `origin/main` reflect the same target that reintegration will validate, reducing confusing diff/status noise from already-landed commits.
