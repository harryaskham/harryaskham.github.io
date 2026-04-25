# Session summary — webapp audit slice 2: stop forwarding worker token

## Goal

Fix the highest-impact dashboard regression surfaced by slice 1 of the
webapp audit: when `caco web` is launched from a managed-worker shell
(every `caco-dev-*` agent), it forwarded the worker-scoped
`CACO_AGENT_TOKEN` to the daemon's UI endpoints and every dashboard
tile rendered "—" / "Snapshot pending" with a silent reconnect loop.

## Bead(s)

- `bd-b6ab99` — caco web forwards worker-scope token, every dashboard
  call returns 403 (P2, claimed by msm-4)
- (parent: `bd-c1c272` — TUI/web audit umbrella)

## Before state

- `dispatch_web` in `crates/caco-cli/src/lib.rs` calls `read_bearer_token`,
  which prefers `CACO_AGENT_TOKEN` (worker scope) over the on-disk node
  token by design (SPEC 7.2.1 — correct for MCP/CLI, wrong for the
  operator-facing dashboard).
- Dashboard cards: `Snapshot pending`, "Reconnecting (n)" cycling, all
  fleet stats `—`. Console: hundreds of `HTTP 403` errors against
  `/api/v1/ui/snapshot` and `/api/v1/ui/stream` (see slice 1
  `summary/0008/`).

## After state

- New helper `read_node_bearer_token_for_web` reads only the
  unrestricted node token from the on-disk runtime directory.
- `dispatch_web` now prefers the node token, falling back to the
  standard `read_bearer_token` resolution (env-first) only if the node
  token is unavailable. The bearer carried into `WebConfig.token` is
  therefore never the worker-scoped agent token, even when `caco web`
  is launched from a managed-agent shell.
- Live verification (single Playwright session, per operator
  preference):
  - `curl /api/v1/ui/snapshot` returns **HTTP 200** with 1.4 MB of
    real snapshot JSON (was 403 before).
  - Dashboard reload via `playwright-cli reload`: console reports
    **0 errors, 0 warnings, 0 occurrences of "403"**.
  - A11y snapshot shows populated cards (Beads/Agents/Services counts
    from live daemon state) instead of `—` placeholders.

## Diff summary

- `crates/caco-cli/src/lib.rs` (+22 / -1):
  - new `read_node_bearer_token_for_web(...)` helper (node-token
    only, returns `Option<String>` so callers fall back gracefully).
  - `dispatch_web` switches to the new helper with a `bd-b6ab99`
    explanatory comment; `read_bearer_token` is unchanged so MCP / CLI
    paths still enforce worker scope per SPEC 7.2.1.
- No test changes in this slice — the unit-test surface for token
  resolution requires a fixture-mounted runtime dir which slice 3
  will set up (planned, scoped to the same bead).

## Embedded artefacts

- `summary/0009/screenshots/web-index-fixed.png` — dashboard rendered
  with real data after the fix; no spinners stuck on "Reconnecting",
  fleet tiles populated.
- `summary/0009/web-index-fixed-snapshot.yml` — Playwright a11y
  snapshot of the same render, showing live counts in nav badges.

## Operator-takeaway

The 403 cascade is gone end-to-end. Slice 1 fixed the visible
keyboard shortcut; slice 2 (this one) fixes the actual data plumbing
underneath. Worker-scope tokens are still strictly enforced for every
other CLI/MCP path. Follow-ups (will be filed as their own beads):
- Front-end: surface 403 from `/api/v1/ui/*` as an explicit
  "insufficient scope" banner instead of generic "Reconnecting (n)" so
  if the same regression ever sneaks back the operator sees it
  immediately.
- Token-resolution unit test with on-disk fixture (planned slice 3 of
  this bead, deferred so this slice can land while the daemon and PR
  pipeline are stable).
