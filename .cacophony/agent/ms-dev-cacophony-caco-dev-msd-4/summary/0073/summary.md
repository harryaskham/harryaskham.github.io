# Session summary — bounded direct message delivery

## Goal

Fix `caco msg send` timing out when direct messaging the `sgu24:nix-on-droid` agent by making direct-message acceptance bounded. The operator-facing goal was to keep the direct-message path usable even when a target node is slow or degraded, without forcing agents to relay the same content through broadcasts.

## Bead(s)

- `bd-a769bc` — caco msg send DM to sgu24:nix-on-droid timing out, forcing manual broadcast relays

## Before state

- Failing tests: none specific to this path; the issue was observed operationally by multiple agents timing out on direct DMs and falling back to broadcast/speak relays.
- Relevant metrics: direct-message delivery awaited cross-node feed fan-out and direct tmux/nudge injection inline after local storage, so a slow target node could block the sender's HTTP/CLI call.
- Context: broadcast and speak paths already used asynchronous fan-out/injection patterns, but `deliver_direct_message` still performed the expensive remote work before returning.

## After state

- Failing tests: none in the validation run.
- Relevant metrics: direct-message delivery now returns after local durable message storage, feed event append, and UI broadcast; cross-node replication and tmux/nudge direct-send injection run in a spawned background task with inbox fallback preserved.
- Context: daemon docs and operator-facing docs now state that direct-send acceptance is bounded and that replication/injection failures are logged rather than requiring broadcast relays.

## Diff summary

- Commits: `710c140bf`
- Files touched: `crates/caco-daemon/src/lib.rs`, `SPEC.md`, `README.md`, `AGENTS.md`, `docs/messaging.html`
- Tests: +1 source-level regression asserting direct-message fan-out/injection remains off the HTTP response path; existing `msg_send_emits_feed_event` rerun; docs validation and daemon checks rerun.
- Behavioural delta: `caco msg send` no longer waits inline for peer fan-out or remote tmux/nudge injection after the message is durably stored, reducing timeout risk for degraded targets while preserving logs and inbox fallback.

## Operator-takeaway

Direct DMs should now fail less noisily during node-specific hiccups: the sender gets a message id promptly, while slower cross-node delivery continues in the background instead of pushing agents toward duplicate broadcast relays.
