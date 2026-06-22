# Session summary — bd-9e60ac: JS classList class-name-styled integrity test (pattern x #5)

## Goal
Apply pattern (x) continuous-defense-test to a fifth audit category: JS classList class-name-styled integrity.

## Bead
- `bd-9e60ac`

## Audit method
1. Scan all .js files for `classList.(add|toggle|remove|contains|replace)('classname', ...)` calls.
2. **Conjoined definition space**: collect class definitions from .css files AND inline `<style>...</style>` blocks in .html files.
3. Compare: orphan = used − defined − allowlist.

## Audit results
- 89 distinct classList class names across 16 .js files.
- 6 truly orphan (visual via inline `.style.X = Y` rather than CSS rules); allowlisted with justifications.

## Allowlist (6 entries)
| Class | Justification |
|-------|--------------|
| `terminal-ime-stable-anchor` | terminal.js IME state marker; visual via inline styles |
| `truncated` | app.js choices-count state flag; no visual rule |
| `workspace-terminal-pane` | workspace-terminal-pane.js pane base; inline styles |
| `wt-floating` | workspace-terminal-pane.js floating-state marker |
| `wt-ime-stable-anchor` | workspace-terminal-pane.js IME-anchor marker |
| `wt-pinned` | workspace-terminal-pane.js pinned-state marker |

## Debugging journey
First test pass found 43 orphans. Investigation revealed two issues:
1. `notifications.html` and `workspace.html` had inline `<style>` blocks not in my html_files list — added.
2. The `defined_anywhere` lookup had a buggy `before_ok` check requiring the char BEFORE the leading `.` to be non-token; this broke compound-selector matching like `.status-pill.is-live` where `is-live` is preceded by the `.pill.` boundary. Fixed by removing `before_ok` — the leading `.` IS the class boundary, so what precedes it is irrelevant.

After fixes, only the 6 true semantic-state markers remained.

## NEW pattern (x) sub-variant: "conjoined-definition-space"
First instance where the pattern aggregates definitions from MULTIPLE source types (CSS files + inline `<style>` blocks in HTML) before subtraction. Demonstrates pattern (x) handles multi-source definition aggregation.

## Pattern (x) generalization (5 audit categories now covered)
| # | Bead | Category | Finds |
|---|------|----------|-------|
| 1 | bd-91616b | CSS custom properties | 2 prunes |
| 2 | bd-a753e2 | @keyframes | 0 (clean) |
| 3 | bd-cf15b7 | ARIA idrefs | 1 a11y fix |
| 4 | bd-f54973 | button-type integrity | 129 fixes |
| 5 | bd-9e60ac | JS classList integrity | 6 allowlisted |

## Operator-visible effect
- Future PR adding `classList.add('foo')` without CSS rule for `.foo` fails CI with remediation guidance.
- Pattern catches both genuine bugs (forgot CSS) and intentional state markers (require allowlist entry).

## Diff summary
- Files touched:
  - `crates/caco-web/src/tests.rs` -- new regression test (~205 lines).
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*`.
- Net pass: 549 -> 550; 0 failures.

## Operator-takeaway
40 cycles, 83 wins. Pattern (x) at 5 audit categories with new "conjoined-definition-space" sub-variant. Catalog: 21 entries (a-q + s + t + u + v + w + x with sub-variants).
