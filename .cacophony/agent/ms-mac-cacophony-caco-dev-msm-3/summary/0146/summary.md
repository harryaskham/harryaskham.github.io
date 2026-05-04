# Session summary — bd-adec17 first-party service disable path

## Goal

Implement `bd-adec17`: add a first-party Cacophony path for the operator/manual-startup policy where the native lifecycle supervisor should be disabled/unloaded, so controllers do not need raw `launchctl disable` / `launchctl bootout` to recover from launchd re-enabling and relaunching the supervisor.

## Bead(s)

- `bd-adec17` — `Add first-party service disable path for manual-startup launchd policy`

## Changes

- Added `caco service disable` to the CLI command metadata and dispatcher in `crates/caco-cli/src/lib.rs`.
- Added `dispatch_service_disable(...)` in `crates/caco-cli/src/service_cmd.rs`:
  - launchd: runs `launchctl disable <scope>/<label>` followed by `launchctl bootout <scope>/<label>`.
  - launchd bootout treats already-unloaded / not-loaded errors (`No such process`, `Could not find service`, `not loaded`, `not found`) as idempotent success.
  - systemd: runs `systemctl --user disable --now <unit>`.
  - supervisord: runs `supervisorctl stop <program>` and reports that persistence is backend-specific.
  - JSON output includes `ok`, backend/unit, per-step command/exit/stdout/stderr/treated_as_success/diagnostics, and guidance.
  - Human output includes step status plus explicit next action: verify `caco service status --json` and run `caco up --skip-update` when daemon/web/TTS services should remain running outside native supervision.
- Added helper functions:
  - `launchd_disable_steps(...)`
  - `launchd_disable_bootout_success(...)`
- Added regression test:
  - `launchd_disable_uses_disable_then_bootout_bd_adec17`
- Updated `SPEC.md` lifecycle-service contract with the `caco service disable` semantics.
- Updated `README.md` command table and lifecycle-service overview.
- Updated `AGENTS.md` Home Manager lifecycle guidance to direct manual-startup recovery through `caco service disable` instead of raw native-manager commands.

## Validation

- `rustfmt --edition 2021 --check --config skip_children=true crates/caco-cli/src/lib.rs crates/caco-cli/src/service_cmd.rs` — passed.
- `git diff --check` — passed.
- `cargo test -p caco-cli bd_adec17 -- --test-threads=1` — passed.
- `cargo clippy -p caco-cli --lib --no-deps -- -D warnings` — passed.

## Notes

- I did not run `caco service disable` live and did not invoke raw `launchctl`; validation is source/test-only to avoid mutating the operator host's current manual-startup state.
