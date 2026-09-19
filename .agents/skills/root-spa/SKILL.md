---
name: root-spa
description: Add, update, validate, and publish standalone static SPAs at a.skh.am root URLs in this repository. Use for a new page like /alex or /par, supplied artwork/audio, static/<slug> directories, root URL mapping, branch previews, or rebuild-safe GitHub Pages deployment. No per-app registry is required.
compatibility: This repository's Jekyll 4 site, Nix default.nix, Python 3, Git, gh, and a real browser for visual checks.
---

# Root-mounted static SPAs

## The contract: a folder is enough

```text
static/foo/index.html  →  _site/foo/index.html  →  https://a.skh.am/foo/
static/foo/app.js      →  _site/foo/app.js      →  https://a.skh.am/foo/app.js
```

**No registry, route table, per-app Jekyll config, or workflow edit is needed.**
The [root-static plugin](../../../_plugins/root_static.rb) discovers Jekyll static
files under `static/` and strips that prefix from their output destinations.
Nested assets retain their paths. There is no second `/static/foo/` deployment.

The live source branch is **main**, not the old `master` checkout. `gh-pages` is
build output. Put durable changes in source, not directly into `gh-pages`.
This repo already has a Jekyll deployment: do not replace it with a separate
publisher or apply another skill's generic `site/` layout.

## Orient first

The repository root is `../../..` relative to the directory containing this
`SKILL.md`. Resolve that path, then run all shell examples below from that root.

1. Check `git status`, the current branch, and `origin/main`; preserve unrelated
   work and use a task branch/worktree from current `main` if necessary.
2. Read the [site conventions](../../../_docs/static-sites.md),
   [source validator](../../../scripts/check-static.py), and
   [deployment workflow](../../../.github/workflows/pages.yml).
3. Choose an unused lowercase kebab-case URL slug from the request. Avoid existing
   site paths such as `assets`, `preview`, `img`, and `static`. The build rejects
   exact output-file collisions rather than silently overwriting other pages.
4. Inspect the supplied assets before choosing layout, palette, or image crops.
   Apply the frontend visual-style skill when available. PAR is a player/code
   reference, not a mandate to reuse its design or personal media.

## Add the page

A typical tree is:

```text
static/foo/
├── index.html
├── style.css
├── app.js
└── assets/
    ├── artwork.png
    └── track.mp3
```

- Every direct child of `static/` must be a directory with an `index.html`.
  Do not put a README, registry, test output, or miscellaneous files at `static/`.
- Use plain HTML **without Jekyll front matter**. Files must already be ready for
  the browser and copy byte-for-byte; no Liquid, layouts, or server-side runtime.
- Use relative asset URLs: `style.css`, `app.js`, `assets/artwork.png`. Do not use
  `<base>`, `/foo/...`, `/static/...`, or root-relative `/assets/...` links. Relative
  URLs also work at `/preview/<branch>/foo/`. Use relative URLs in CSS, fetches,
  imports, and dynamically created media too; inspect those manually because the
  link checker cannot discover all JavaScript-generated paths.
- GitHub Pages is a static host, **not a history-router fallback**. Prefer a
  single page or hash routes (`/foo/#/settings`). A request to `/foo/settings`
  returns 404 unless a matching static file/directory exists.
- Keep it dependency-free when practical. If using a bundler, configure relative
  asset paths and place only its browser-ready output in `static/foo/`; keep
  authoring sources outside the published tree. No `node_modules`, secrets,
  credentials, local settings, diagnostic output, or private review screenshots.
- No symlinks or hidden/underscore-prefixed path components. The validator catches
  files Jekyll would otherwise skip. Framework output requiring such paths needs
  an intentional pipeline change, not a pretend registry entry.
- Current media limits: **each file <10,000,000 bytes; all `static/` files combined
  <25,000,000 bytes**. Check the validator for current limits. Optimize oversized
  assets or explicitly review a budget change; do not bypass validation. Alex has
  additional existing clip/hash checks that must continue to pass.
- Set a useful title, viewport, accessible image alt text, and local favicon.
  Keep metadata URLs pointed at the intended live URL, not a local test host.
- For audio, start `audio.play()` directly from a user gesture. Do not await a
  fetch first or promise sound on page load. Use native `loop` for a repeating
  track, pause/retry controls, and native audio controls as the no-JS fallback.
  See [PAR's player](../../../static/par/player.js) and
  [HTML](../../../static/par/index.html) for the established pattern.

Everything placed in the app directory is intended to become public. Keep agent
skills in `.agents/skills/`, notes in `_docs/`, and browser tests in `_tests/`,
not in an app's asset tree. The deployment must continue to exclude agent folders.

## Validate locally

```sh
python3 scripts/check-static.py
nix-shell --run './scripts/build'
nix-shell --run 'make dev'
# Open http://127.0.0.1:4000/foo/
```

The build runs the [routing regression tests](../../../scripts/check-static-routing.rb),
then verifies every source asset matches its root-mounted output. It covers
preview base URLs, same-second edits, deletion cleanup, and output collisions.
The validator discovers new folders automatically; **do not add the slug to it**.
Add app-specific tests only when the new app has behavior worth testing.

If the configured `<nixpkgs>` registry is unavailable, use an existing nixpkgs
checkout with `nix-shell -I nixpkgs=/absolute/path/to/nixpkgs --run ...`. Do not
hard-code another machine's Nix store path into the repo or globally install gems
as a shortcut. This is a `default.nix` project, not a `nix run` flake.

Render the real page at desktop, narrow phone, and short/landscape sizes. Capture
and inspect screenshots, not just the DOM. Check artwork aspect ratio, clipping,
overflow, primary action, touch/keyboard access, focus, and reduced motion. Exercise
loading/error recovery and no-JS fallback where relevant. For audio, test actual
playback, pause/resume, seeking, and end-to-start looping; use a byte-range-capable
server such as Jekyll/WEBrick (Python's basic `http.server` is insufficient for
reliable seek/loop tests).

Use an isolated named Playwright session and keep screenshots outside the publish
output. [PAR's browser check](../../../_tests/par-browser.js) is a reference, but
it contains PAR-specific selectors, metadata, dimensions and audio expectations;
do not run it unchanged against a different app. Smoke-test `/alex/` and `/par/`
if shared routing or deployment code changes. Stop owned test servers and browsers.

## Publish when requested

1. Review the diff and run the checks. Commit the app and any relevant tests/docs
   on a branch based on current `main`; land it on `main` using the requested Git
   workflow. Do not overwrite concurrent work or force-push.
2. The existing `pages.yml` workflow builds Jekyll and publishes to `gh-pages`,
   preserving branch previews and `CNAME`. Non-main branches publish previews;
   their path is `/preview/<branch-with-slashes-replaced-by-->/foo/`.
3. Watch the source deployment **and** Pages build. A successful Git push alone
   is not proof that the page is live.
4. Verify HTTPS HTML and at least one real media URL at `https://a.skh.am/foo/`.
   Confirm new output, not a stale cached page. Verify live interactions where
   applicable and that the existing home/subsites still work.
5. Report the live URL, source path, commit, checks performed and any remaining
   browser/platform limitations. Do not claim untested platforms passed.

Do not change DNS, the custom domain, or Pages source settings merely to add a
folder. Future rebuilds retain the new app because its assets live on `main`.
