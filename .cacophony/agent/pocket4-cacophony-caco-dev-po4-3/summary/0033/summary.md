# S6: suggest `/run` endpoint — node-local, guarded execution (bd-bd6157)

## Bead
bd-bd6157 — caco suggest S6: `/suggest/<uuid>/<option>/run` endpoint,
tmux-per-uuid, run-once. Slice 6 of the `caco suggest` epic (bd-a84d20). Depends
on S5 (persistence/run-state, landed `536662283`) + S4 (parse).

## What changed
New `crates/caco-daemon/src/suggest/run.rs` (registered `pub mod run;`), plus the
`POST /api/v1/suggest/<uuid>/<option_uuid>/run` route in the daemon Router (all
listener tables).

This is the **only** path in the suggest epic that executes anything. Generating
never executes (S3); this explicit, node-local endpoint does, under strict guards.

### Guards (all enforced before any execution)
1. **High-risk DENY backstop** (`high_risk_reason`) — a Rust port of the
   managed-worker PreToolUse high-risk patterns
   (`plugins/caco-agent/agents/intercept-direct-commands.sh`): privilege
   escalation (sudo/doas), firewall mutation (nft/iptables/ufw/firewall-cmd/
   pfctl), service restart/stop/disable, broad process kills (pkill/killall/
   kill -9/-N), destructive `.git`/`.cacophony`/`/daemon`/`/agents` `rm -rf`,
   `git push --force[-with-lease]`, `git reset --hard`, and nested shell
   wrappers (`bash -lc "sudo …"`). A blocked item is reported non-runnable with
   a reason (HTTP 403 `suggest_run_denied_high_risk`) — never executed.
2. **Run-once** — an already-run item is refused (HTTP 409
   `suggest_run_already_run`) unless it set `allow_multiple_runs == true`.
   Safety (high-risk) is evaluated first so a denied command can never run even
   if multi-run.
3. **Node-local** — always executes on the triggered node (Harry's decision).
   No run-forwarding in the suggest layer; an `@othernode` token is just ordinary
   `caco @node` runtime forwarding at exec time.

### Execution + tracking
- `type=bash` → `sh -c <command>`; `type=caco_cli` (or unknown, conservatively)
  → argv exec (`build_invocation`).
- `tmux_socket_name(uuid)` → `caco-suggest-<uuid>` for the per-UUID observability
  socket.
- Captures stdout+stderr into `<root>/suggest/<uuid>/runs/<option_uuid>/output.log`
  (node-local, never replicated).
- Updates `run_count`, `last_run_at`, `last_run_status` (ok|error) in the
  persisted `suggestions.json` (S5's `ItemRunState`).
- Response: `{ uuid, option_uuid, status, exit_code, run_count, logs_dir }`.

## Slice boundary — feed/replication is S5b (msm-1, bd-2bff13)
Coordinated with msm-1 (S5b owner): S6 updates **local run-state only** and adds
**no EventType**. Both the `SuggestSetCreated` (generate) and `SuggestRun`
(post-run, live-status-only) feed `EventType` variants + emission + the bounded
full-state `suggest_sets` wiring all land together in S5b, so every feed/
replication EventType change reviews in one slice. msm-1 will rebase onto S6 to
pick up the `run.rs` run-state shape before adding emission.

## Core invariant honored
**Only this explicit endpoint executes; generating never does.** The high-risk
DENY backstop + run-once gate run before any process spawn.

## Tests
9 new unit tests (35 total in the suggest module), all green via the daemon queue
(`RUST_MIN_STACK=33554432 cargo test -p caco-daemon --lib suggest:: -- --test-threads=1`):
- `high_risk_denies_sudo_and_firewall`, `high_risk_denies_service_and_kills`,
  `high_risk_denies_destructive_git_and_rm`, `high_risk_denies_nested_shell_wrappers`
- `safe_commands_allowed` (read-only diagnostics + non-destructive rm/reset pass)
- `run_gate_order_high_risk_first` (safety beats multi-run)
- `run_gate_run_once`
- `build_invocation_bash_vs_caco_cli`
- `tmux_socket_name_per_uuid`

## SPEC areas
SPEC 6.x daemon endpoint + execution-control surface; epic bd-a84d20 central
safety contract (suggesting never runs; explicit-run-only; high-risk DENY reuse;
run-once). No SPEC contract change — additive guarded endpoint.

## Diff summary
New run module + 1 route (×4 tables). Final landed squash SHA per the
reintegration receipt.
