# Session summary — SuccessEnvelope-aware fetch unwrap audit (bd-76db0a)

## Goal

Audit `crates/caco-web/static/*.js` for `fetch()` call sites that
read response fields directly off the JSON body (silently
returning `undefined` because the daemon wraps `/api/v1/*`
responses in `SuccessEnvelope { ok, data: <T>, request_id, meta }`).
Land a shared `apiUnwrap()` helper and convert the broken /
fragile sites to use it.

## Bead(s)

- `bd-76db0a` — Audit caco-web JS modules for SuccessEnvelope-aware
  fetch unwrap (P3, audit/refactor; filed via reflect-session
  from bd-451cfe)

## Before state

- bd-451cfe surfaced the bug class: `timeline.js` read `json.events`
  instead of `json.data.events` and the timeline rendered empty.
- ~80+ `/api/v1/*` fetch sites across `app.js`, `terminal.js`,
  `timeline.js`, `workspace-*.js` — most defensive, some not.
- Only two places had factored helpers: `workspace-views.js::jfetch`
  (returns `json.data` directly — best pattern) and
  `workspace-chat-pane.js::fetchJSON` (returns full body, callers
  apply ad-hoc `(env && env.data) || env || {}`).

## Audit findings

Scripted scan of all `await fetch('/api/v1/...').json()` sites
classified each by what it reads off the parsed body:

| Site | File:line | State | Action |
|---|---|---|---|
| `/api/v1/nodes/{node}` | app.js:4406 | **BROKEN** — reads `data.load_summary` directly; daemon wraps in SuccessEnvelope so this was always `undefined`, falling to `data.telemetry` (also undefined) → `null`. Silent. | Fixed via `apiUnwrap` |
| `/api/v1/agents/{id}/logs` (TTY poll) | app.js:5417 | Fragile but working: `data.tmux_capture \|\| data.data?.tmux_capture` | Routed via `apiUnwrap` |
| `/api/v1/agents/{id}/artefacts` | app.js:5886 | Defensive: inline `(body && body.data) \|\| body \|\| {}` | Routed via `apiUnwrap` |
| `/api/v1/agents/{id}/artefacts/summary/{i}?file=` | app.js:5942 | Same defensive inline | Routed via `apiUnwrap` |
| `/api/v1/agents/{id}/logs` (tab) | app.js:5975 | Fragile but working: `data.logs \|\| data.data?.logs` | Routed via `apiUnwrap` |
| `/api/v1/agents/{id}/diff` | app.js:5998 | Fragile but working: `data.diff \|\| data.data?.diff` | Routed via `apiUnwrap` |
| All other `/api/v1/*` sites in app.js | various | Either ignore body (POSTs that just check `resp.ok`) or already use envelope-aware extraction | No change |
| `workspace-views.js::jfetch` returns `json.data` directly | n/a | Best pattern | Kept |
| `workspace-chat-pane.js::fetchJSON` + ad-hoc unwrap | n/a | Working | Kept (file owns its own helper) |
| `terminal.js`, `timeline.js`, other workspace-*.js | n/a | Already envelope-aware (bd-451cfe + earlier sweeps) | No change |

## After state

New shared helper added in app.js near `el()`:

```js
function apiUnwrap(body) {
    if (body == null || typeof body !== 'object') return {};
    if (body.data && typeof body.data === 'object') return body.data;
    return body;
}
```

Six call sites converted: nodes/{node} (real bug fix) +
agents-logs ×2 + agents-artefacts + agents-artefacts-summary +
agents-diff (all fragile-defensive → uniform).

## Diff summary

- Files touched:
  - `crates/caco-web/static/app.js`:
    - Added `apiUnwrap` helper (~22 LOC)
    - Converted 6 fetch call sites
    - Real bug fix at /api/v1/nodes/{node} (was silently reading undefined)
  - `crates/caco-web/src/tests.rs`:
    - New `app_js_uses_api_unwrap_helper_for_success_envelope_sites`
- Tests: +1 / -0
  - Pins: helper exists; helper handles SuccessEnvelope branch;
    nodes/{node} call site routes through apiUnwrap (regression-pin
    via window-scan around the URL literal); ≥6 apiUnwrap sites
    overall (smoke check that the migration stuck).
- Test command: `cargo test -p caco-web app_js_uses_api_unwrap` → 1 passed.

## Operator-takeaway

- **Real bug fixed**: webapp Nodes view's per-node telemetry/scheduling
  panel was silently empty for any non-local node where the daemon
  wraps the response (which is all of them, as of current daemon).
  Now populates correctly.
- **Helper in place**: future fetch sites can use `apiUnwrap(await
  resp.json())` instead of repeating defensive ad-hoc unwraps.
- **Out-of-scope follow-ups (NOT closed by this bead)**:
  - Standalone helper files (`workspace-chat-pane.js::fetchJSON`,
    `workspace-views.js::jfetch`) intentionally retained — each
    encapsulates its own error-handling. Unifying these into
    `apiUnwrap` would be a larger refactor; not the scope of this
    audit. (If pursued, file separately as a refactor bead.)
  - The ~70 remaining `/api/v1/*` POST sites that only check `resp.ok`
    don't need apiUnwrap — they discard the body. Audit confirms
    this is intentional.

Honored constraints:
- `cargo test -p caco-web app_js_uses_api_unwrap` only — no workspace test.
- Pre-close audit will run before close.
- Operator close-discipline: real bug fixed + 5 fragile sites
  hardened in a single landed change; no out-of-scope work
  silently buried.
- Operator `bd update --status=closed` bypass directive: ACK,
  using only `caco bd close` (with `--admin-override --reason`
  for any non-landed legitimate close).

21st bead closed this session (cumulative). 14th in this turn.
