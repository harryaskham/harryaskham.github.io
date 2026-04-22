# Session summary 0033 — bd-6a50ec: caco bootstrap dev --start-daemon (slice 1)

## Goal

Cover step 3 of bd-ce32fa scope (idempotent daemon launch) so a
fresh dev can run `--init-config` → `--start-daemon` → `--check`
to land at a working daemon without touching launchd/systemd.

## Bead(s)

- `bd-6a50ec` slice 1 — daemon-start only.

## Before state

- `caco bootstrap dev` accepted only `--check` / `--init-config`.
- New devs had to run `caco supervisor`, `caco up`, or install a
  launchd unit by hand to start the daemon.

## After state

- `caco bootstrap dev --start-daemon`:
  - Probes `GET /api/v1/health` (any HTTP response = alive).
  - If alive: no-op, exit 0.
  - Otherwise shells out to `caco up --skip-update`, then re-probes
    for up to 5s (10 × 500ms).
  - Surfaces `caco up` stdout/stderr in the output for diagnostic.
  - `--json` returns `{ok, data:{already_running, health_url,
    action, up_exit_status, now_running, up_stdout, up_stderr}}`.
  - Exit code 1 if daemon still unreachable after retry window.
- Independent of `--check` / `--init-config`.

## Diff summary

- Commit: `db2a4c71`.
- Files (1): caco-cli lib.rs (+131 lines).
- `cargo build` and `cargo clippy` for caco-cli + caco-daemon: clean.

## Operator-takeaway

Three-command bootstrap now works:

    caco bootstrap dev --init-config
    caco bootstrap dev --start-daemon
    caco bootstrap dev --check

Slice 2 (`--join PROJECT` / `--create-project` / `--demo-agent`)
is the remaining bd-6a50ec work and is filed as a follow-up.
