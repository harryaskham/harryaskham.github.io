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
| `static/eddie/` | <https://a.skh.am/eddie/> |
| `static/jay/` | <https://a.skh.am/jay/> |

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

Eddie's birthday page crossfades ten 4K WebP montages every 10 seconds. Phones
and very wide displays use the whole image over a blurred backdrop so the headline
stays visible. A click-to-play, looping YouTube embed with sound sits centred at
30% viewport height on landscape screens and below the headline on portrait
phones. Its browser check uses a fake clock to cover the full loop, pause/resume,
reduced motion, image failure, video placement and no-JS fallback:

```sh
playwright-cli -s=eddie-check run-code --filename=/absolute/path/to/_tests/eddie-browser.js
```

Jay is a fully local medical transcription PWA. Speech recognition runs in a
module worker on ONNX Runtime Web (WASM SIMD, threads when cross-origin isolated);
audio, transcripts and history live in IndexedDB and never leave the browser.
Moonshine Tiny (MIT) and the CPU WASM runtime ship with the page as verified,
gzip-sharded `.bin` files; MedASR and Moonshine Base are optional one-time
downloads cached in Cache Storage. Regenerate or verify the vendored bytes with:

```sh
python3 _tools/jay-assets.py          # rebuild static/jay/{runtime,models}
python3 _tools/jay-assets.py --check  # verify committed shards
python3 _tools/jay-stamp.py           # REQUIRED after editing static/jay: restamp the build id
```

GitHub Pages compresses some files on the fly (including `.bin`), so the
local dev server is not a faithful CDN: always run `_tests/jay-smoke.mjs`
against the live site after a deploy.

Every Jay shell file carries one content-derived `jay-build:<id>` stamp. The
service worker caches each build atomically (refusing a half-propagated deploy),
serves it consistently offline, and the page reloads onto a new build only when
idle; HTML/script mismatches heal with one reload. `scripts/check-static.py` runs
each `_tools/*.py --check`, so CI fails if the stamp is stale.

On-device model weights and runtimes under `static/<name>/models/` or
`static/<name>/runtime/` use a separate reviewed 40 MB budget so they do not
consume the 25 MB media budget; the 10 MB per-file limit still applies. Jay's
service worker is scoped to `/jay/` only. It adds COOP/COEP headers for WASM
threads and caches the app shell for offline use. It never handles requests
outside `/jay/`, and it bypasses `/jay/worklog/`.

```sh
JAY_URL=http://127.0.0.1:4000/jay/ node _tests/jay-browser.mjs      # end-to-end (fake mic)
JAY_URL=http://127.0.0.1:4000/jay/ node _tests/jay-reliability.mjs  # recovery, backup, watchdog
JAY_URL=http://127.0.0.1:4000/jay/ node _tests/jay-update.mjs       # deploy consistency, self-heal
JAY_URL=http://127.0.0.1:4000/jay/ node _tests/jay-streaming.mjs    # clean-audio live streaming
JAY_URL=http://127.0.0.1:4000/jay/ node _tests/jay-mobile.mjs       # share target, back gesture
JAY_URL=http://127.0.0.1:4000/jay/ node _tests/jay-mic.mjs          # mute, mic disconnect, interruption
MEDASR_DIR=… JAY_URL=… node _tests/jay-downloads.mjs                 # flaky network, gzip CDN, cancel
node _tests/jay-smoke.mjs   # AFTER EVERY DEPLOY: live CDN build consistency + new-visitor cold load
JAY_URL=http://127.0.0.1:4000/jay/ node _tests/jay-crossbrowser.mjs # Firefox + WebKit
```
