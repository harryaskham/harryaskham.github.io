# Session summary — bd-874f33: IME guard on remaining text-input Enter handlers

## Goal

Audit remaining text-input Enter handlers for the bd-34d83f
IME composition guard. bd-34d83f originally established
the pattern but only applied it to the two handlers
in-scope at the time (TUI sidebar connect, chat-pane
slash-suggest); subsequent template additions didn't carry
the guard forward.

## Bead(s)

- `bd-874f33` — [caco-web] add bd-34d83f IME composition guard to remaining text-input Enter handlers (#agent-nudge-input, #agent-tty-search)

## Audit categorization

Found 14 `event.key === 'Enter'` handlers total. Categorised:

| Category | Count | Action |
|---|---|---|
| Already IME-guarded (bd-34d83f) | 2 | Preserved |
| Button-like activations (Enter \|\| Space on rows/cards/stats) — NOT text input | 9 | Keep unguarded (no IME context) |
| Cmd/Ctrl+Enter textarea (refineQuickBead) — IME doesn't commit with modifier | 1 | Keep unguarded (modifier semantics intact) |
| **Text-input Enter — UNGUARDED** | **2** | **Fixed by bd-874f33** |

The two fixed sites:

1. **`#agent-nudge-input`** — Enter sends a message to an
   agent. Chat-style input; IME use is common.
2. **`#agent-tty-search`** — Enter runs the terminal
   search. IME users searching for Japanese/Chinese log
   fragments would re-fire searches per IME commit.

## Pattern applied

```html
<!-- before -->
onkeydown="if(event.key==='Enter'){...}"

<!-- after (bd-874f33) -->
onkeydown="if(event.key==='Enter'&&!event.isComposing&&event.keyCode!==229){...}"
```

The `event.keyCode !== 229` complements `!event.isComposing`
— some browsers (older Safari, some Firefox versions)
report keyCode 229 during IME composition instead of
setting `isComposing`. Both checks together cover all
modern browser behavior.

No-op for non-IME users: the additional conditions only
become false during active IME composition.

## Test gotcha sidestepped (bd-5e0030 defense-in-depth)

The bd-5e0030 footgun has fired 3 times. This time used
`format!()` string concatenation in the test source to
construct the forward-guard "bare antipattern" needle, so
the literal antipattern doesn't appear as a contiguous
string in `tests.rs` source:

```rust
let bare_nudge = format!(
    "id=\"agent-nudge-input\"{}{}",
    " class=\"agent-nudge-input\" placeholder=\"...\" maxlength=\"2000\" ",
    "onkeydown=\"if(event.key==='Enter'){event.preventDefault();sendAgentNudge("
);
assert!(!app_js.contains(&bare_nudge), "...");
```

Defense-in-depth if a future broader-scope guard ever
scans `tests.rs` itself. (The footgun has historically
fired when the antipattern was in a comment in the file
being scanned, but the pattern of avoiding literal
antipatterns in test source is generally safer.)

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted to avoid self-referential SHA.
- Files touched:
  - `crates/caco-web/static/app.js` -- 2 inline onkeydown attribute extensions (#agent-tty-search line 8375, #agent-nudge-input line 8879).
  - `crates/caco-web/src/tests.rs` -- regression test pins both guards + forward-guards for both old shapes + count-guard preserving the existing bd-34d83f spaced-form guards.
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*` -- bounded validation receipts.
- Tests: +1 caco-web static asset regression test. Net pass count: 478 -> 479; 11 pre-existing failures on main unchanged.

## Operator-takeaway

Japanese, Chinese, Korean, and Vietnamese IME users can
now type in the agent-nudge input and terminal-search box
without their Enter-to-commit accidentally firing send /
search mid-composition. Combined with the prior 22
perf/polish wins this session, all 4 text-input Enter
handlers in the dashboard are now uniformly IME-safe.
