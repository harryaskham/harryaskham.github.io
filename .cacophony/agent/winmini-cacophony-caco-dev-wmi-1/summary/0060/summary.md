# Session summary — bd-3656ce: caco sidecar list/status differentiated; --service validated

## Goal

`caco sidecar list` and `caco sidecar status` produced
byte-identical output (list was `dispatch_sidecar_status`
under another name).  Make `list` actually do the
config-only thing its help text promised, and stop
`sidecar start/stop --service <unknown>` from silently
swallowing the flag (bd-b76723 family).

## Bead(s)

- `bd-3656ce` — P3 bug, test-user-hel filed.

## Before state

- `sidecar list` and `sidecar status` printed the same
  `sidecar status for node: <n>` heading + same rows.
- `sidecar start --service nonexistent` accepted the
  flag, ignored the value, printed the configured-services
  list as if everything were fine.
- `sidecar stop --service nonexistent` same.

## After state

- `dispatch_sidecar_list` is now its own implementation:
  - heading: `configured sidecar services for node: <n>`
  - one line per configured service name (no runtime probe)
  - footer hint: ``(use `caco sidecar status` for live
    desired/actual state)``
  - JSON shape: `{ ok, node, services: [String] }`
  - works even if the daemon is offline (no LifecycleManager
    runtime poll).
- `dispatch_sidecar_start` and `dispatch_sidecar_stop`
  validate `--service <svc>` against
  `mgr.configured_services()`. Unknown values now error:
  `unknown service '<svc>' for node '<n>'. Configured:
   <list> (or omit --service for all)`.
- Help text for `list` updated to `"List configured
  services for this node (config-only; no runtime status
  — see 'sidecar status')."`

## Diff summary

- 1 file touched, +66 / −6:
  - `crates/caco-cli/src/lib.rs`:
    - rewrote `dispatch_sidecar_list` (was 4-line alias).
    - added --service validation block to start + stop.
    - updated SIDECAR_SUBCOMMANDS list-leaf description.

## Verification

- `cargo build --bin caco`: clean.
- Live (winmini, daemon up):
  - `sidecar list` → config-only heading, 1 service, footer hint.
  - `sidecar status` → unchanged runtime-probe heading + row.
  - `sidecar start --service nonexistent` → error (was: silent).
  - `sidecar start --service caco-daemon` → original output (passes validation).

## Operator-takeaway

Family with bd-b76723 (silent flag swallow) +
bd-126b99/bd-a403a1/bd-30fbfb/bd-2886bb/bd-dfc91a/bd-215e3f/bd-c81d1a/bd-c061d4
(CLI honesty pass).  Two takeaways:

1. When `dispatch_X = dispatch_Y` becomes "we'll figure
   out what X means later", file a follow-up — leaving
   them aliased indefinitely confuses operators reading
   help text.
2. Any `_param: Option<&str>` parameter is a candidate
   for silent-swallow audit — grep
   `_service_filter|_filter|_unused` for similar dead
   wiring elsewhere.
