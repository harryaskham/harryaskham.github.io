# Session summary — webapp audit slice 3: explicit 'Insufficient scope' banner for 401/403

## Goal

Close the loop on the deferred follow-up of `bd-b6ab99` (slice 2): if
the dashboard ever talks to a daemon endpoint that returns 401 or 403,
surface it as an explicit, sticky, operator-readable state instead of
the generic "Reconnecting (n)" cycle that masked the slice-1 / slice-2
regression for so long.

## Bead(s)

- `bd-834dd5` — caco-web: surface 401/403 from `/api/v1/ui/*` as
  explicit "Insufficient scope" state (P3, claimed by msm-4)
- (parent: `bd-c1c272` — webapp audit umbrella)
- (related: `bd-b6ab99` — slice 2, closed)

## Before state

- `loadSnapshot` treated all non-2xx responses as a generic network
  error: `setConnectionStatus('disconnected')` + retry every 5 s.
- The connection-status pill rendered "Reconnecting (n)" / "Disconnected"
  with no distinction between "daemon unreachable" and "daemon present
  but rejecting our token".
- `setConnectionStatus` had no `forbidden` branch.
- During slice 1 / slice 2 this exact symptom hid a 403 cascade behind
  a perpetual reconnect spinner.

## After state

- `loadSnapshot` now branches on `resp.status === 401 || resp.status === 403`
  and calls `setConnectionStatus('forbidden')` before throwing.
- The catch block no longer downgrades an already-set `forbidden`
  status to `disconnected` on the next retry tick — it stays sticky.
- `setConnectionStatus` has a new `'forbidden'` case:
  - reuses the `disconnected` red dot styling so colour-only consumers
    still get the right gestalt,
  - sets the pill text to **"Insufficient scope"**,
  - tooltip explains the cause ("Daemon rejected dashboard request
    (HTTP 403). The bearer token in use lacks node scope.") and the
    remediation ("Restart caco web from a non-managed-worker shell,
    or unset `CACO_AGENT_TOKEN` so the on-disk node token is used.").
- New one-shot error toast on transition into `'forbidden'` so the
  state can't be missed by an operator who isn't already looking at
  the header pill.
- Regression test
  `app_js_surfaces_403_as_explicit_forbidden_state` asserts every
  invariant of the new flow against the bundled `app.js`, so the
  same drift gets caught at unit-test time next time.

## Diff summary

- `crates/caco-web/static/app.js` (+36 / -2): snapshot fetch path,
  catch-block sticky check, `'forbidden'` branch, transition toast.
- `crates/caco-web/src/tests.rs` (+32 / 0): new regression test.

## Test status

- New test authored; not executed locally per operator's
  "tests-killed-machine" guidance. The assertions are pure substring
  checks against `static/app.js` embedded via `StaticAssets::get`, so
  they will pass on the CI runner (every asserted substring is
  present in the diff above).
- No behavioural change for any non-401/403 path (`disconnected`,
  `cached`, `connecting`, `connected` cases unchanged).

## Operator-takeaway

Slice 2 made the dashboard work; slice 3 makes sure that if it ever
breaks the same way again, the operator sees **"Insufficient scope"**
instead of an infinite "Reconnecting (n)" loop. Also fires a loud
toast on transition. No remaining slice-3 follow-ups for this bead.

Webapp audit umbrella `bd-c1c272` is in good shape after these three
slices: keyboard contract honest, dashboard data plumbing fixed, and
a clear failure mode for the same regression. Next candidates already
identified in slice 1 / slice 2 summaries:
1. Walk Beads / Feed / Chat / Nodes / Services / Workspace nav (now
   that the dashboard actually loads) and file UX beads as discovered.
2. Investigate `bd-1ef80b` (PR-mode reintegration succeeds locally
   but never reaches forge) — operator-impacting infra bug.
