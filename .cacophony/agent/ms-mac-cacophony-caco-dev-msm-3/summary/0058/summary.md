# Session summary 0058 — bd-fc60ff doctor sensor

## Goal

Land acceptance criterion #4 of bd-fc60ff (doctor sensor for git
coredumps inside the cacophony service group). Criteria #1 (atomic
write) and #2 (flock around git_commit_journal) had already landed;
bead was closed by another worker.

## Bead(s)

- `bd-fc60ff` — already-closed; this commit lands the missing
  doctor-sensor criterion as a follow-up reflect.

## Before state

- check_git_in_cacophony_coredumps did not exist.
- git SIGBUS coredumps were silent to log-monitor, silent to
  daemon.log post-v1.2.519, only visible via systemd-coredump.
- timeline_pipeline.rs had broken-on-main clippy errors blocking
  workspace clippy.

## After state

- New runtime-area doctor check 'git coredumps in last 1h':
  ok/warning/error tiers; Linux-only with coredumpctl probe.
- timeline_pipeline.rs builds clippy-clean.

## Diff summary

- Commit: c11ff286e892
- Files: `crates/caco-cli/src/lib.rs` (+72 doctor sensor),
  `crates/caco-daemon/src/timeline_pipeline.rs` (drive-by clippy)

## Operator-takeaway

bd-fc60ff is closed. If git SIGBUS recurs after the atomic-write +
flock landed, 'caco doctor' on Linux will now surface the count
without needing 'caco service logs | grep coredump'.
