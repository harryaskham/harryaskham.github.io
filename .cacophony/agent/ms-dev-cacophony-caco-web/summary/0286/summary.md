# Session summary — bd-d8a9f8: chat-body 966px overflow root-caused + fixed

## Goal

Resolve the long-investigated caco-web chat-body horizontal overflow (a specific
"ESCALATING…" message extended .chat-body/.chat-message-content to scrollWidth=966
at both wide and narrow widths). md2-0 had ruled out every overflow-wrap config
across 5 observe cycles and documented it as "not solvable by blind CSS word-break
edits — needs live DOM inspection," but hit chromium tooling friction. They handed
it off to me (I have working chromium tooling).

## Bead(s)

- `bd-d8a9f8` — caco-web: chat-body overflows ~966px from a non-text structural
  child (claimed via handoff from md2-0 + closed this session). Follow-up of the
  landed bd-799444.

## Before state

- .chat-body scrollWidth=966 (constant wide/narrow), overflowX=visible, from a long
  inline element that overflow-wrap could not break. Root unknown after 5 cycles.

## After state

- ROOT-CAUSED via a live-DOM probe (real renderChatBody + real CSS rendered in
  chromium at 320px, computed-style enumeration of every child wider than the
  container): an inline `<code>` (from `` `…` `` — e.g. the "gh workflow run …"
  command in the message) computed `white-space: nowrap` and measured w=1138,
  overflow=818. The global `:not(pre) > code` rule (style.css ~9627) sets
  white-space: nowrap on inline code, and nowrap DEFEATS overflow-wrap — exactly
  why every prior overflow-wrap edit failed.
- FIX (targeted, low-collision): `.chat-body code { white-space: pre-wrap;
  overflow-wrap: break-word; }` overrides the global nowrap for chat inline code
  ((0,1,1) beats (0,0,2)), so long inline commands wrap instead of overflowing;
  short `<code class="bead-ref">` bd-id chips keep `white-space: nowrap` (stay
  intact, they are short and never overflow). The global inline-code nowrap is
  preserved everywhere else (this is a scoped chat override, not a global flip).
- VERIFIED: re-running the probe after the fix → inlineCodeLong overflow 818→0,
  ALL candidates (longUrl, longToken, inlineCodeLong, manyBeadRefs, escalating)
  overflow=0, no wide children. Needle guard added.

## Diff summary

- Code commit: pending (final landed squash SHA from the reintegration receipt).
- Files: crates/caco-web/static/style.css (.chat-body code wrap + bd-ref nowrap),
  crates/caco-web/src/tests.rs (needle guard). No app.js change, no wasm change.
- Tests: +1 needle guard (chat_body_inline_code_wraps_not_nowrap_bd_d8a9f8).

## Operator-takeaway

The chat-body overflow was a CSS specificity/cascade trap, not a word-break gap:
the global `:not(pre) > code { white-space: nowrap }` (intentional for short code
chips) silently defeated every overflow-wrap fix on long inline commands. The live
-DOM computed-style probe (real renderChatBody + CSS in chromium) pinpointed it in
one pass where 5 cycles of blind CSS edits could not. Fix is a scoped .chat-body
override that keeps short bd-ref chips intact. Reusable lesson: when overflow-wrap
"does nothing," check the element's computed white-space for an inherited nowrap.
