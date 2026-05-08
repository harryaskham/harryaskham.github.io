# Session summary — TLS fatal-alert stderr recurrence

## Goal

Fix the post-`bd-e5e022` recurrence where `daemon-crash.log` could still grow from routine cluster diagnostics, specifically TLS handshake lines whose peer-alert wording contains the word "fatal" even though the daemon itself did not crash.

## Bead(s)

- `bd-130bc7` — Replication and cluster diagnostics still write to daemon-crash.log after bd-e5e022 close

## Before state

- Failing tests: none known for this bead.
- Relevant metrics: ms-mac log-monitor observed `daemon-crash.log` grow by 21,375 bytes during a healthy daemon/service window, with no panic/OOM/fatal daemon-crash lines.
- Context: `bd-e5e022` added sidecar stderr routing, but the classifier checked the generic `fatal` crash keyword before recognizing known routine TLS/cluster diagnostics. TLS libraries report peer alerts as `fatal alert`, which made those routine connection diagnostics look crash-like.

## After state

- Failing tests: none known for this bead.
- Relevant metrics: focused sidecar tests now assert TLS and beads TLS `fatal alert` handshake failures route to `daemon.log`, not `daemon-crash.log`.
- Context: known routine daemon diagnostics are classified before generic crash-keyword matching, preserving real panics/fatal runtime errors while excluding TLS peer-alert chatter from the crash log.

## Diff summary

- Commits: `9a88b13dcd`, `4002c411a9`
- Files touched: `crates/caco-sidecar/src/lifecycle.rs`
- Tests: added `daemon_stderr_router_tls_fatal_alert_is_not_crash_bd_130bc7` and expanded the non-crash diagnostic fixture.
- Behavioural delta: TLS handshake diagnostics that include peer "fatal alert" text are now routed as routine daemon diagnostics instead of crash evidence.
- Validation: `tj-5bedee24` passed `cargo test -p caco-sidecar bd_130bc7 -- --nocapture`; `tj-0534b3c9` passed the full `daemon_stderr_router` focused subset; `tj-dc13b08e` and `tj-692f6220` passed the same focused subset after rebases.

## Operator-takeaway

The recurrence was a classifier-ordering bug: "fatal alert" in TLS protocol diagnostics is not a daemon crash, so routine cluster handshake failures now stay out of `daemon-crash.log` while unknown or actual crash-like stderr remains conservative crash evidence.
