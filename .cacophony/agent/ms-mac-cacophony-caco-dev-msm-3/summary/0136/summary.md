# Session summary — launchd disabled-label bootstrap repair

## Goal

Continue reopened `bd-2d3244`: post-install verification showed the previous diagnostics landed, but `caco service load` still failed with launchd bootstrap exit 5 and the supervisor remained `not_loaded`.

## Bead(s)

- `bd-2d3244` — Alert and repair when native lifecycle supervisor is installed but not loaded

## Changes

- Diagnosed the remaining live blocker with bounded diagnostics:
  - `launchctl print-disabled gui/501` showed `"com.cacophony.lifecycle" => disabled`.
  - Source-built `caco service status --json` surfaced `enabled:false` before repair.
- Updated `crates/caco-daemon/src/native_supervisor.rs` so launchd detection parses `launchctl print-disabled` and reports `enabled:false` / `enabled:true` when available.
- Updated `crates/caco-cli/src/service_cmd.rs` so launchd `caco service load` runs `launchctl enable gui/<uid>/<label>` before bootstrap and kickstart.
- Extended launchd diagnostics to explain enable failures and to mention disabled-label state as a cause of bootstrap exit 5.
- Updated `SPEC.md`, `README.md`, and `AGENTS.md` to document launchd enable + bootstrap + kickstart as the installed-but-not-loaded repair path.

## Validation

- `rustfmt --edition 2021 --check --config skip_children=true crates/caco-cli/src/service_cmd.rs crates/caco-daemon/src/native_supervisor.rs` — passed.
- `CARGO_BUILD_JOBS=2 cargo test -p caco-cli bd_2d3244 -- --test-threads=1` — passed.
- `CARGO_BUILD_JOBS=2 cargo clippy -p caco-cli --lib --no-deps -- -D warnings` — passed.
- `git diff --check` — passed.
- Source-built live repair proof: `CARGO_BUILD_JOBS=2 cargo run -p caco -- service load --json` ran `launchctl enable`, `bootstrap`, and `kickstart`; all three exited 0, and follow-up `service status --json` reported `healthy:true`, `active_state:"running"`, `enabled:true`.
- Installed/current `caco service status --json` after the source-built repair reported `healthy:true` and `active_state:"running"`.

## Known validation gap

- Attempted `CARGO_BUILD_JOBS=2 cargo test -p caco-daemon launchd_print_disabled_parser_reports_disabled_label_bd_2d3244 -- --test-threads=1`, but the existing caco-daemon test target still fails to compile before reaching the new test because multiple pre-existing `Config { ... }` test initializers are missing the newer `macos` field. This is the same pre-existing daemon-test blocker observed in earlier work.

## Coordination notes

- Claimed the reopened bead only after my own local strict board gate turned green.
- Used first-party `caco service` for repair proof; raw `launchctl` use was limited to bounded diagnostics (`print-disabled` and plist inspection), not recovery.
