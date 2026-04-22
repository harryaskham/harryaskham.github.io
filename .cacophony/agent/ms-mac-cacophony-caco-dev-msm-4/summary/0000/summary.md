# bd-bd60cb — extend runtime launch timeout to `pi` runtime

## Goal
Stop the recurring `Persistent agent caco-dev-msm-N failed during
periodic reconcile` cascade (16 occurrences today on ms-mac).

## Bead(s)
- bd-bd60cb (P2 bug). Filed by log-monitor with the hypothesis that
  the failures cascade from helsinki authoritative-bd 502 blips.

## Diagnosis
Cross-referenced the bead's hypothesis with the actual `feed.jsonl`
detail strings on this node:

  agent launch failed: daemon error: agent ms-mac-cacophony-caco-dev-msm-N:
  bootstrap completed, but runtime launch failed: tmux session did
  not settle on a launched runtime command (observed 'bash') for
  configured agent runtime 'pi'.

This is the post-bootstrap settle check in
`crates/caco-daemon/src/agent/lifecycle.rs ~L1562`, not a 502 from
`/beads`. Same shape as bd-a6a3f2 for codex: an npm-installed
Node.js runtime whose wrapper chain (bash → npm shim → node →
pi-cli) doesn't complete within the default 2 s window when the
host is under load. The bd-048e5b transient-retry path covers
tmux probe hiccups while the session is alive — it does not
extend the wrapper-completion budget.

The helsinki blip story still correlates: helsinki probe storms +
sync 502 retries push host load up, which in turn pushes the `pi`
wrapper past 2 s. So helsinki is the precipitating load, not the
direct error path.

## Before state
- `runtime_launch_timeout_secs("codex") == 10` (bd-a6a3f2).
- `runtime_launch_timeout_secs("pi")` fell through to default == 2.
- Persistent `caco-dev-msm-*` agents (which run the `pi` runtime)
  cascaded into reconcile failures whenever the host was loaded.

## After state
- `crates/caco-daemon/src/agent/mod.rs`: `runtime_launch_timeout_secs`
  matches `"codex" | "pi"` to the 10 s NODEJS bucket. Doc comments
  updated to name both runtimes and reference bd-bd60cb.
- New test `runtime_launch_timeout_pi_extended` asserts pi → 10 s
  and pi > claude. Existing codex_extended + default tests untouched.

## Diff summary
- `crates/caco-daemon/src/agent/mod.rs` (+13/-5): one-line match arm
  change + expanded doc comments.
- `crates/caco-daemon/src/agent/tests.rs` (+17/-1): one new test in
  the existing bd-a6a3f2 cluster.

## Tests
- `cargo build -p caco-daemon` — clean.
- `cargo clippy -p caco-daemon --all-targets -- -D warnings` — clean.
- `cargo test -p caco-daemon --lib runtime_launch_timeout` — 3/3.

## Operator-takeaway
After binary roll, persistent caco-dev-msm-* (and any other `pi`
runtime persistent agents — node-ctrl, etc.) should stop cascading
reconcile failures during helsinki blips. The fix is the same shape
as bd-a6a3f2: extend the npm-wrapper budget. If similar failures
appear for another node-wrapped runtime, the same one-line change
(add it to the codex|pi match arm) applies.
