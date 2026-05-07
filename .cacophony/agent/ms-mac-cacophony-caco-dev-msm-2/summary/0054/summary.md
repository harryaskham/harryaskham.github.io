# Session summary — service login identity for beads sync

## Goal

Fix bd-971979 by making ms-mac supervised Cacophony services launch git/ssh subprocesses with a stable login identity environment, so beads sync no longer fails with `No user exists for uid 501` after launchd/systemd-managed restarts.

## Bead(s)

- `bd-971979` — ms-mac beads_sync git fetch fails with No user exists for uid 501

## Before state

- Failing tests: not applicable at session start; the live symptom was daemon log WARNs from `beads_sync` git fetches.
- Relevant metrics: bead evidence reported 69 WARN/HTTP-5xx-class lines in a 15-minute ms-mac daemon log window, with repeated `git fetch origin beads failed: No user exists for uid 501` across multiple projects.
- Context: the revived checkout already contained one implementation commit, later rebased to `02243397`, adding service identity environment propagation. A one-time consistency check found this agent running, `bd-971979` as the only in-progress claim for this agent, the bead checkout fresh (`↑0 ↓0`), and the expected one-commit diff.

## After state

- Failing tests: none observed in targeted validation.
- Relevant metrics: `caco log tail --service caco-daemon --lines 300` still contained older uid-501 errors up to `2026-05-07T07:36Z`, but no later uid-501 entries; after the restart window, the remaining matching `beads_sync` lines were transient SSH connection closures around `2026-05-07T07:49Z`.
- Context: the fix keeps Home Manager/native service units and lifecycle-spawned children aligned on `HOME`, `USER`, and `LOGNAME`, in addition to the existing deterministic `PATH`/`CACO_BIN` contract. `caco service status --json` reported the native launchd supervisor healthy; `caco bd status` reported authoritative ms-mac beads routing fresh with `↑0 ↓0`.

## Diff summary

- Commits: `02243397` (implementation commit after first-party rebase; final landed squash SHA to be assigned by reintegration receipt).
- Files touched: `crates/caco-cli/src/lib.rs`, `crates/caco-sidecar/src/lifecycle.rs`, `flake.nix`, `SPEC.md`, `README.md`, `docs/nix.html`, `.cacophony/agent/ms-mac-cacophony-caco-dev-msm-2/summary/pending/summary.md`.
- Tests: +0/-0 test files; extended existing unit coverage for service wrapper environment and managed service child environment.
- Behavioural delta: generated systemd/launchd wrappers now include `USER` and `LOGNAME`; Home Manager service env now exports `HOME`, `USER`, and `LOGNAME`; lifecycle-spawned managed child services preserve the supervisor's identity environment for git/ssh subprocesses.
- Validation: `RUST_MIN_STACK=33554432 cargo test -p caco-cli generated_service_wrappers_include_managed_tool_path --lib` passed; `cargo test -p caco-sidecar start_service_passes_managed_path_and_caco_bin_env --lib` passed; `./docs/validate-pages.sh` passed with 3313 checks; `nix build .#checks.aarch64-darwin.hm-module-eval --no-link` passed.

## Operator-takeaway

The root fix is to stop treating launchd/systemd service children as if they always have a login-shell identity. The code now makes identity variables explicit wherever Cacophony launches or materializes supervised services, which should prevent Nix/OpenSSH git from failing uid lookup during beads sync after restarts. I also filed draft `bd-f69be7` for a separate status-surface inconsistency observed during recovery: `caco status` can still report daemon down while `caco bd status`, `caco log tail`, and `caco service status` are usable.
