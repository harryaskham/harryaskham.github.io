# Session summary — bd-07121d: defer all blocking <script> tags

## Goal

Continue the caco-web frontend perf/visual/UX polish loop with a
high-impact perceived-performance fix: every external `<script>`
in the dashboard was parser-blocking, so the browser could not
paint or fire DOMContentLoaded until 700+ KiB of vendor + first-
party JS had downloaded and executed.

## Bead(s)

- `bd-07121d` — [caco-web] add defer to all blocking <script> tags (perf: FCP/TBT)

## Before state

`crates/caco-web/static/index.html` loaded 13 external scripts at
end-of-body with no `defer` or `async`:

- 5 xterm vendor bundles (`xterm.min.js` + 4 addons)
- `workspace-overlay.js`
- `app.js` (585 KiB)
- `nodes.js`
- `kitty-graphics.js`
- `workspace-terminal-pane.js`
- `timeline.js`
- `summaries.js`
- `workspace-integrated.js`
- `workspace-panes.js`

Without `defer`, each tag blocked HTML parsing while it downloaded
and executed. The browser could not paint anything below the script
tags or fire DOMContentLoaded until every script had finished.
First Contentful Paint and Total Blocking Time both took the full
serial cost of vendor + first-party JS.

## After state

- Every external `<script src="...">` tag in `index.html` carries
  `defer`. The python pass also caught one tag I had missed in my
  manual count: total is now 14 deferred tags, all in document
  order. The browser now downloads them in parallel while it
  continues parsing HTML, and executes them in order just before
  firing DOMContentLoaded.
- `defer` is the correct attribute (not `async`) because document
  order matters: `workspace-overlay.js` must run before `app.js`,
  xterm bundles must run before `workspace-terminal-pane.js`.
- No script in the bundle uses `document.write` (verified via grep
  across all of `crates/caco-web/static/*.js`), and all init code
  is attached on DOMContentLoaded, which still fires correctly
  after deferred scripts execute (see `app.js:158`, `app.js:11930`,
  `app.js:12140`).

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted to avoid self-referential SHA.
- Files touched:
  - `crates/caco-web/static/index.html` — added `defer` to 14 external `<script src=...>` tags.
  - `crates/caco-web/src/tests.rs` — added `all_external_scripts_have_defer_bd_07121d` asserting every external script tag has `defer`/`async`, count floor of 13, and no inline `<script>` blocks. Updated `caco_web_nodes_renderer_lives_in_focused_static_module_bd_0fe376` to expect the new `defer` form of the nodes.js shard load.
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*` — bounded validation receipts including the full `<script` tag listing.
- Tests: +1 caco-web static asset regression test; existing test updated to follow the new attribute. Net pass count: 430 -> 431; failure count unchanged at 11 (all pre-existing on main).

## Operator-takeaway

The browser can now parse HTML and start painting the dashboard
shell in parallel with downloading and executing 700+ KiB of JS,
instead of stalling on a serial vendor+app.js download. First
Contentful Paint and Total Blocking Time both improve, especially
on slower networks. Existing script execution order is preserved,
so xterm, workspace-overlay, and app.js dependencies still resolve
correctly.
