# bd-5b02d6 — caco agent reintegrate broken-on-main pre-push gate

## Goal
Catch broken-on-main waves at their source — the agent that introduced
the wave — by simulating the rebase onto the latest target tip and
re-running the workspace test+clippy gates one final time before the
squash-merge push.

## Bead(s)
- bd-5b02d6 (P2 feature) — broken-on-main mitigation: caco agent
  reintegrate should rebase + re-test immediately before squash-push.

## Before state
- An agent runs full test+clippy at HEAD-of-branch, then by the time
  the squash-merge push lands main has moved. The push-rebase pulls in
  new files (e.g. new struct fields, new dead-code targets) which
  would have failed locally if tested. The wave then ripples to every
  active worker who pulls main (~5–15 minutes of peer-repair time
  per wave; bd-ee1696 reported 13+ waves in a single session).
- `dispatch_agent_reintegrate` proceeded straight from preflight +
  before_reintegration hooks into `reintegrate(&req)` with no
  rebased-state validation step.

## After state
- New `broken_on_main_gate_enabled()` env-controlled toggle
  (`CACO_REINTEGRATE_BROKEN_ON_MAIN_GATE` ∈ `1`, `true`, `yes`, `on`,
  case-insensitive). Defaults off until we have enough field
  experience to make it the default.
- New `run_broken_on_main_gate(checkout, target_branch, remote)` runs:
    1. `git fetch <remote> <target_branch>`
    2. snapshot `HEAD` for unconditional rollback
    3. `git merge --no-edit -X theirs FETCH_HEAD` (working tree only)
    4. `cargo test --workspace --lib --no-fail-fast`
    5. `cargo clippy --workspace --all-targets -- -D warnings`
    6. always `git reset --hard <pre_merge_head>`
- When step 4 or 5 fails the gate returns the captured failure detail
  and `dispatch_agent_reintegrate` aborts with a clear message,
  refusing to push the squash.
- A merge conflict in step 3 is treated as non-blocking — the daemon's
  auto-rebase loop will still handle it, so we report `Ok` and let it
  run.
- Three new tests cover the toggle parser (default-off, truthy-on,
  falsy-off); the helper itself is exercised end-to-end by enabling
  the env var on the next reintegration.

## Diff summary
- `crates/caco-cli/src/lib.rs`:
  - In `dispatch_agent_reintegrate`, gate-check between
    `ScopedEnvVar::set("CACO_REINTEGRATION_AUTO_REBASE_RETRY_LIMIT", …)`
    and the call to `reintegration::reintegrate(&req)`.
  - New helpers `broken_on_main_gate_enabled()` and
    `run_broken_on_main_gate(checkout, target_branch, remote)`.
  - New tests `broken_on_main_gate_disabled_by_default`,
    `broken_on_main_gate_enabled_by_truthy_env`,
    `broken_on_main_gate_disabled_by_falsy_env`.

## Operator-takeaway
Set `CACO_REINTEGRATE_BROKEN_ON_MAIN_GATE=1` for an agent (in profile
`environment` or via the launcher) to opt in to the pre-push gate.
Cost is one extra full workspace test+clippy run per reintegration
(~30s on warm caches, more on cold caches). When the gate trips, the
agent gets the failure output verbatim and a `Fix and re-run.`
instruction; the squash never lands. Pairs with bd-2c399b (full
merge-queue daemon), which remains the proper architectural fix.

## Tests
- `cargo test -p caco-cli --lib broken_on_main_gate` — 3 passed.
