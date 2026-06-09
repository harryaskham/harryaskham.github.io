# Session summary — durable per-agent preserve marker (node-local)

## Goal

Replace the all-or-nothing CACO_RETROACTIVE_SOCKET_DEAD_CLEANUP env gate
(bd-7beb2f) with a surgical, durable per-agent preserve marker so the
socket-death reconcile sweeps skip a pinned diagnostic/bisect subject (e.g. the
86r5 bd-b03101 target under the bd-074eac do-not-disturb directive) while the
sweep can run for everyone else.

## Bead

- `bd-a81baf` (P2, daemon/daemon-resilience/lifecycle) — durable per-agent
  preserve marker for reconcile-transition exclusion (follow-up to po4-1's
  bd-7beb2f). Coordinated: po4-1 (filer) + winmini stood down, both endorsed
  node-local; msm-3 (bd-7189fc/bd-b03101 reconcile-path owner) informed.

## Design decision

Took the NODE-LOCAL sidecar marker over the typed AgentInfo.preserve field. The
bead originally scoped a typed field requiring a #[derive(Default)] / ~250
construction-site refactor across types.rs/lib.rs/replication.rs/tests.rs — a
hot-main rebase minefield. po4-1 confirmed the replication question: each daemon
only reconciles/transitions its OWN agents, so a marker on the agent's home node
fully protects it (same locality as the bd-074eac directive it replaces).
Replicated cross-node pin visibility is a later UX nicety, not a correctness
need. Node-local = correct + low churn + zero struct churn.

## Before state

- bd-be60bb (new-edge) + bd-7beb2f (retroactive) socket-death transitions had NO
  per-agent exclusion; bd-7beb2f was gated behind an opt-in env flag (default
  off) that had to stay off until any pinned diagnostic subject resolved.

## After state

- New node-local preserve marker (the pause-stamp pattern):
  health::write_preserve_stamp / remove_preserve_stamp / preserve_stamp_present
  using $TMPDIR/caco-worker-hooks/<agent_id>.preserve.
- Both pure decision functions gained an is_preserved param and skip preserved
  agents: runtime_sweep::should_fail_socket_dead_managed_agent (bd-be60bb) and
  should_retroactively_fail_stuck_socket_dead (bd-7beb2f).
- Wired into both lifecycle.rs sweep call sites (filter + under-lock re-check).
- caco agent set --field preserve --value true/false sets/clears the marker
  (AgentManager::set_field as a sidecar write, no AgentInfo field); caco agent
  get --field preserve reads it. CLI forwards --field generically, so no CLI
  change needed.
- Env gate left default-OFF in this change: relaxing it to default-on requires
  the operator/doctor to first pin 86r5 (ms-mac-node-local) with the marker;
  that flip is deferred to the reconcile owners (msm-3) since I can't write
  ms-mac's node-local marker from a headless pocket4 worker. The marker
  mechanism is the enabler for that flip.

## Diff summary

- Code commits: pending final squash SHA from the reintegration receipt.
- Files: crates/caco-daemon/src/agent/{runtime_sweep.rs (is_preserved params +
  doc + tests), health.rs (preserve stamp helpers + round-trip test),
  lifecycle.rs (sweep call sites + set_field/get_field preserve)}.
- Tests: +preserve cases on both decision functions + a health stamp round-trip;
  existing socket-death tests updated for the new arg. Queued daemon test
  tj-fdf82d3b.

## Operator-takeaway

`caco agent set --id <agent> --field preserve --value true` now pins an agent
(node-local) against the socket-death reconcile sweeps; `--value false` unpins.
Once pinned diagnostic subjects carry the marker, the reconcile owners can relax
the bd-7beb2f env gate toward default-on. Avoided the 250-site AgentInfo
refactor entirely.
