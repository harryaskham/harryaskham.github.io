# Standalone sites

The live source branch is **main**, not the old master branch. GitHub Pages serves
`gh-pages`, which the existing Jekyll deployment rebuilds from source.

For the complete add/test/publish workflow, use the repository skill
[**root-spa**](../.agents/skills/root-spa/SKILL.md).

**No registry or per-page configuration is required.** Keep each standalone page
in `static/<name>/index.html`, with its own relative CSS, scripts, images and audio
links; the plugin and validator discover new directories automatically:

| Source | Live URL |
| --- | --- |
| `static/alex/` | <https://a.skh.am/alex/> |
| `static/par/` | <https://a.skh.am/par/> |

`_plugins/root_static.rb` strips `static/` from static-file destinations during
both `jekyll build` and `jekyll serve`. It does not add redirects or a second
`/static/` copy, and it rejects collisions with normal site output. The same
files work under `/preview/<branch>/<name>/` because all asset links are relative.
Keep standalone HTML free of Jekyll front matter; these files are copied verbatim.
Hidden/underscore paths and symlinks are intentionally disallowed by validation.
This is static hosting, not a server-side SPA fallback: use hash routing or create
real static entrypoints for deep links such as `/foo/settings/`.

## Local development and checks

```sh
nix-shell --run 'make dev'       # http://127.0.0.1:4000/par/ (also /alex/)
nix-shell --run './scripts/build'
python3 scripts/check-static.py # source, media budgets, local links + Alex hashes
```

The build script and deployment validate the root mapping and compare every
static asset byte-for-byte against the generated site. Routing tests also cover
branch-preview base URLs, same-second edits, deletion cleanup and output collisions.
The static-file mapping retains sub-second mtimes so fast local edits are not lost
to Jekyll 4's whole-second modification-time cache.

The Nix environment uses `<nixpkgs>` from your Nix configuration. If that configured
registry is unavailable, `nix-shell -I nixpkgs=/path/to/a/cached/nixpkgs --run ...`
can use an existing nixpkgs checkout without changing the project or global setup.

PAR has no external fonts, scripts or runtime dependencies. Its supplied PNG and
MP3 live beside the HTML. Playback begins on Play, repeats through the native
`audio` loop attribute, supports pause and seeking, and falls back to browser audio
controls without JavaScript. Media errors offer a retry and a direct MP3 link.

For real-browser checks, open `/par/` in a named Playwright CLI session, then run:

```sh
playwright-cli -s=par-check run-code --filename=/absolute/path/to/_tests/par-browser.js
```

This verifies playback, pause/resume, looping, seeking, rapid input, network error
recovery, reduced motion, and representative viewport sizes. Run it against local
build output before publication; final verification must also check live HTTPS.
