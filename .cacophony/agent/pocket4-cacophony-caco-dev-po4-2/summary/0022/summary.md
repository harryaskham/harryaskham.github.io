# Session summary — ms-mac TTS restart path documented after operational convergence

## Goal

Close out the ms-mac TTS daemon restart incident after the local operators had
already recovered the live service. The remaining work was to document the
canonical first-party recovery path so future operators do not repeat the
`kill + nohup` pattern that temporarily left the daemon process state,
feed-watcher expectations, and control-port observations in a confusing place.

## Bead(s)

- `bd-a606d7` — ms-mac TTS daemon: feed-watcher / control-port disconnected after kill+nohup restart — speaks not reaching daemon (zero counters, no trace)

## Before state

- During route-flip work on ms-mac, an ad hoc `kill` + `nohup caco tts daemon`
  attempt left the TTS situation confused:
  - stale/duplicate helper shells
  - status reading looked inconsistent
  - trace/counters temporarily failed to reflect live speaks
- The bead explicitly asked for the canonical supervised restart path to be
  documented so this operator workflow would not be repeated.
- Shortly after, ms-mac-local operators recovered the service correctly and
  confirmed convergence:
  - single canonical TTS daemon PID
  - route `local-device`
  - device `MacBook Pro Speakers`
  - fresh probes reaching `terminal outcome=played`

## After state

- The live ms-mac TTS daemon is operationally converged.
- The docs now make the first-party recovery contract explicit:
  - use `caco tts daemon status`
  - use `caco tts io output show`
  - use `caco tts io output set --mode local-device` when the operator wants
    sound from the Mac's speakers
  - inspect the managed lifecycle with `caco service status`
  - recover via `caco up` / `caco restart`
- The docs also explicitly warn against `kill` + `nohup` for managed-node TTS
  recovery, reserving `caco tts daemon` in the foreground for intentional debug
  use only.

## Diff summary

- Files touched:
  - `docs/macos-development.md`
  - `README.md`
- Validation:
  - docs-only change; no code/test surface changed
- Behavioural delta:
  - future ms-mac TTS incidents now have a documented first-party recovery path
    and a clearer distinction between live route flips and supervised daemon
    recovery

## Operator-takeaway

The live ms-mac TTS issue converged operationally once the route was set to
`local-device` and the real daemon was healthy again. The durable fix in this
bead is documentation: operators now have an explicit first-party recovery path
that avoids ad hoc `nohup` restarts and points them at the right status, route,
and trace commands for managed nodes.
