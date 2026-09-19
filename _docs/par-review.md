# PAR visual and interaction review

## Direction

High-contrast, screen-printed, stripped-back. The supplied square sleeve is the
only hero; a diagonal red band with white edges extends its palette into the
backdrop. Square edges, a paper-colored transport strip, bold sans play/pause
labels and compact monospaced times. No headline duplication, external fonts,
framework, visualizer, automatic motion, or autoplay.

## Rendered review

Screenshots retained in `/tmp/askham-par-review/` for this implementation session;
desktop/mobile finals are also in the Pi image preview gallery.

| Frame | Finding / action |
| --- | --- |
| `par-1440x960.png` | Artwork is uncropped, dominant and centered. The transport shares the sleeve's edges; red/white diagonals balance the black side fields. |
| `par-390x844.png` | Cover, Play target, timestamps and seek bar remain visible without scrolling. The backdrop maintains the same diagonal motif. |
| `par-landscape-before.png` | A wide but short viewport produced a small sleeve with desktop-sized controls; readout overflowed. Fixed with container-based compact controls and reduced vertical padding. |
| `par-844x390.png` | Re-capture: complete cover and all controls fit; readout no longer overlaps. |
| `par-320x568.png` | Narrowest tested width retains the full art and readable, non-overlapping timestamps. |
| `par-keyboard-390x844.png` | Play focus has a visible inset outline; Space toggles playback and arrows seek. |
| `par-error-390x844.png` | Failed media preserves the sleeve and controls, announces a short error, and exposes retry/direct MP3 access. |
| `par-no-js-390x844.png` | Native audio fallback remains usable with JavaScript disabled. |

## Checks

- Real Chromium rendering at 1440×960, 390×844, 320×568, 1280×600 and 844×390;
  zero horizontal/vertical overflow in the normal state, square image preserved,
  readout bounds checked. No page JavaScript errors.
- Supplied MP3 actually decoded and advanced. Play, pause/resume, seeking, keyboard,
  rapid start/cancel, native end-to-start loop, blocked MP3 recovery, reduced motion
  and no-JavaScript fallback all passed (`_tests/par-browser.js`).
- Use Jekyll/WEBrick or another byte-range-capable server for seek/loop tests.
  Python's basic `http.server` does not implement byte ranges; it rendered correctly
  but its media seeking test failed. The same tests passed on real Jekyll output
  with HTTP 206 range responses.
- Alex's original 50 audio hashes and all local links passed validation. Real
  browser Play/Stop passed after migration; its hard-coded base URL was removed
  so relative assets also work in branch previews.
- Nix Jekyll production build passed; every static file matched its root-mounted
  output byte-for-byte, with no `/static/` duplicate. Routing tests covered preview
  base URLs, same-second edits, deletion cleanup, and rejection of output collisions.
  CI exposed Jekyll 4's whole-second mtime cache; the mapping preserves timestamp
  precision, and a deterministic regression test fails under the old behavior.
- Safari/WebKit was not tested: its Playwright browser is not installed locally.
