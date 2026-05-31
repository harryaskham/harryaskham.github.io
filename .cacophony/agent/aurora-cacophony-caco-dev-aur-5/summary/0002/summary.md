# Session summary — Managed Pi launch uses native --name flag (bd-2ca846)

## Goal

Operator-requested (Harry): switch managed Pi launch from injecting a leading
`/session-name <agent-id>` initial message to Pi's native `--name <agent-id>`
(`-n`) CLI flag. The injection approach coupled session naming to the prompt
stream and could let a naming-command warning interfere with the real task
instructions; the native flag is processed before any positional prompt, so the
managed task/nudge prompt stays the first real instruction.

## Bead(s)

- `bd-2ca846` — Use Pi native --name/-n session-naming flag in managed launch
  instead of injecting /session-name command (task; managed-launch, pi,
  session-naming, operator-requested)
- `bd-a7a854` — (draft, reflect-session) Worker in_progress claims can silently
  drop to open during sync/recreate windows

## Before state

- Failing tests: none related; this was a refactor, not a bug fix.
- Managed Pi launch injected `"/session-name ${CACO_AGENT_ID}"` as a Pi initial
  message via `managed_pi_session_name_arg()` at 5 call sites in
  `crates/caco-daemon/src/agent/spawn.rs` (fresh launch, fresh relaunch, and the
  resume/continue/`--session` rewrite helpers).
- AGENTS.md / SPEC.md / README.md documented the `/session-name` injection.
- Pi ships native `--name` / `-n` (verified in Pi 0.78 docs: sessions.md,
  rpc.md, README) working across interactive, print, JSON, and RPC modes.

## After state

- Failing tests: none. Targeted lanes green via queue: `inject_pi_*` (7/7) and
  the broader `pi_init` / `pi_session` / `pi_continue` / `pi_fresh` /
  `build_resume_init_script_pi` set (37/37). Clippy on caco-daemon lib: exit 0
  (only pre-existing unused-import warning, unrelated).
- Managed Pi launch now emits `'--name' "${CACO_AGENT_ID}"` in flag position
  (before --append-system-prompt and the positional goal on fresh launches;
  inserted right after the `'pi'` token, idempotently, on resume/continue
  rewrites). The legacy `/session-name` message is no longer injected, and
  `strip_managed_pi_session_name_arg` strips both forms so previously-rewritten
  init scripts migrate cleanly on resume.
- Docs updated across AGENTS.md, SPEC.md, README.md (×2).

## Diff summary

- Code/content commit: e3adb04848 (final landed squash SHA from reintegration
  receipt).
- Summary artefact commit: intentionally omitted.
- Files touched: `crates/caco-daemon/src/agent/spawn.rs` (new
  `managed_pi_name_flag_arg` / `insert_pi_name_flag_after_pi`, updated 5 call
  sites + stripper), `crates/caco-daemon/src/agent/tests.rs` (assertions →
  `--name` contract), `AGENTS.md`, `SPEC.md`, `README.md`.
- Tests: ~6 test functions updated to the new contract; 0 added, 0 removed.
- Behavioural delta: managed Pi sessions are named via `--name` flag instead of
  a `/session-name` initial message; session display name unchanged (stable
  agent ID), but naming no longer rides the prompt stream.

## Embedded artefacts

None.

## Operator-takeaway

Managed Pi launches now use Pi's first-class `--name` flag for session naming
instead of injecting a `/session-name` chat command, eliminating the documented
fragility where a naming-command warning could swallow the real task prompt.
Verified the flag exists and is mode-complete in the shipped Pi docs before
switching. All managed launch shapes (fresh, relaunch, resume, continue,
--session) and the docs contract are covered. Separately filed bd-a7a854 after
hitting (and seeing aur-1 hit) silent in_progress claim loss during sync/recreate
windows — worth investigating since it can cause double-claims.
