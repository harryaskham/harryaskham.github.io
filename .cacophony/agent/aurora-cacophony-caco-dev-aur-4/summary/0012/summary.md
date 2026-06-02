# Session summary — expose running-daemon version + build commit in caco status --json

## Goal

Close an observability gap surfaced during a live health diagnosis: `caco status --json`
returned `.daemon.version = null` and exposed no running-daemon build commit, so answering
"is the RUNNING daemon behind a landed fix?" required process/binary/git archaeology
(supervise start time vs installed-binary commit date vs origin/main dates). Make that a
one-read check by reporting the live daemon's code version and build commit directly.

## Bead(s)

- `bd-e99814` — Expose running daemon code commit/version in caco status --json (.daemon.version is null) (P3 task)

## Before state

- `caco status --json` `.daemon` had no `version`/`commit` fields; `.daemon.version` was effectively null.
- `/api/v1/node` already served `version` (CARGO_PKG_VERSION) but the CLI status object never surfaced it.
- caco-daemon consumed `option_env!("CACO_GIT_HASH")` (e.g. auto_restart.rs) but caco-daemon/build.rs never baked it, so it resolved to None/"unknown".
- Failing tests: none (pre-existing reintegration::tests broken-on-main bd-9f33ab is unrelated and owned by aur-2).

## After state

- `/api/v1/node` `NodeInfo` now also serves `commit` (short git hash) when known.
- `caco status --json` `.daemon` now reports `version` and `commit` of the LIVE daemon process (not the invoking CLI binary), so installed-CLI vs running-daemon drift is directly readable — supporting caco ops / doctor stale-daemon detection.
- Validation on the rebased tree: `cargo check -p caco-daemon -p caco-cli` = 0 errors; `cargo clippy -p caco-daemon -p caco-cli --lib` = 0 warnings (queued job tj-b2751129).
- Failing tests: none introduced.

## Diff summary

- Code commit: see reintegration receipt for the final landed squash SHA (local pre-rebase commit was authored after a multi-hour local daemon outage during which the work was preserved uncommitted, then committed + rebased clean onto latest main).
- Files touched: `crates/caco-daemon/build.rs` (+30), `crates/caco-daemon/src/lib.rs` (+19), `crates/caco-cli/src/lib.rs` (+54/-5). Total +98/-5.
- caco-daemon/build.rs: bake `CACO_GIT_HASH` (`git rev-parse --short=9 HEAD`) + `CACO_COMMIT_TIMESTAMP`, matching caco-cli/caco-tui build.rs, with `rerun-if-changed` on `.git/HEAD` and `.git/refs/`.
- caco-daemon NodeInfo: add `commit: Option<&'static str>` via new `daemon_build_commit()` helper (filters empty/"unknown"); `#[serde(skip_serializing_if = "Option::is_none")]`.
- caco-cli: thread the running daemon's `version` + `commit` from `/api/v1/node` through the status gather (`AsyncGatherResult`) into `DaemonStatus.{version,commit}`, both `skip_serializing_if = Option::is_none`.
- Tests: +0 / -0 / flipped 0 (additive observability field; existing status-shape tests unaffected).
- Behavioural delta: `caco status --json` `.daemon` gains `version` + `commit` for the live daemon when the daemon is reachable and built with git metadata; omitted otherwise (no misleading placeholder).

## Operator-takeaway

`caco status --json .daemon` now tells you the exact code version and build commit the
LIVE daemon is running, distinct from the CLI binary you invoked it with. The next time a
node looks wedged or "behind a fix", you can compare `.daemon.commit` to the landed fix
commit in one read instead of correlating process start times against git history. Note:
the running daemon only reports its own `commit` once it has been rebuilt+restarted with
the new build.rs (older running daemons omit the field, which itself signals "pre-fix daemon").
