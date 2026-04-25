# Session summary — caco-web /summaries route

## Goal

Add the web-dashboard surface for the session-summary viewers epic so
operators can browse summaries from any browser — including mobile
devices over Tailnet — with the same NORD-themed two-pane design as
the TUI viewer.

## Bead(s)

- `bd-a0503e` — caco-web: /summaries route with asciinema playback
- parent epic `bd-a5e2fa`
- depends on `bd-41b916` (closed), `bd-ba7239` (closed)

## Before state

- caco-web had no `summaries` view or any awareness of the
  `/api/v1/summaries` endpoints.
- VALID_VIEWS had 14 entries, viewKeys lacked an `s` binding.
- No `.summaries-*` CSS classes or JS module existed.

## After state

- New sidebar entry **Summaries** (📒 icon, key `s`) between
  Timeline and Merge Queue.
- `summaries.js` (~430 lines, self-contained IIFE):
  * Lazy fetch on view-open, filter controls for project / agent /
    bead-ID.
  * Agent-grouped list with sticky headers, relative timestamps,
    bead-ID chips, artefact icons (🎬📸{}).
  * Detail pane renders all seven canonical sections with NORD
    accent-colored left borders.
  * Bead chips link to `#beads?q=<id>` for cross-view navigation.
  * Asciinema affordance: shows CLI command to play + copy-path;
    inline CDN playback deferred to future raw-file endpoint.
- `summaries.css` (~260 lines): fully scoped under `#view-summaries`,
  responsive grid (stacks on <900px), sticky agent group headers.
- `index.html`: new sidebar `<li>`, `<div class="view"
  id="view-summaries">`, CSS + JS link tags.
- `app.js`: `summaries` added to VALID_VIEWS, `s` key shortcut,
  `renderSummaries()` hook in switchView.
- `cargo test -p caco-web --lib`: 239 passed.

## Diff summary

- Files touched: 5
  * `crates/caco-web/static/summaries.js` (new, ~430 lines)
  * `crates/caco-web/static/summaries.css` (new, ~260 lines)
  * `crates/caco-web/static/index.html` (+sidebar entry, +view div,
    +CSS/JS links)
  * `crates/caco-web/static/app.js` (+VALID_VIEWS entry, +key
    shortcut, +switchView hook)
- Tests: +0 / -0 (JS view is integration-tested via browser; no
  server-side changes)
- Behavioural delta: new operator-facing view accessible at
  `http://<host>:11180/#summaries`.

## Operator-takeaway

The web viewer is 100% client-side — it proxies to the daemon's
`/api/v1/summaries` endpoints through caco-web's existing
`/api/{*rest}` reverse proxy. No new server routes were needed. The
`.summaries-*` CSS namespace avoids style collisions with the rest of
the SPA. If you want inline asciinema playback, a future bead should
add a `GET /api/v1/summaries/<agent>/<idx>/raw/<file>` endpoint to
stream terminal.cast / screenshots / data.json through the proxy;
the JS already has a `playCast()` hook ready to consume it.
