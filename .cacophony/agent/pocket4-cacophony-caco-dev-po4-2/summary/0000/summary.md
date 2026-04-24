# Session summary — `caco scratch list` JSON payload right-sized

## Goal

Cut the default `caco scratch list --json` payload back down to the
shape callers actually want: lightweight metadata-only by default,
with explicit opt-ins (`--include-content`, `--content-preview N`)
when a caller really needs the body. The pre-fix response inlined
every note's full body, making the JSON 40x larger than the text-mode
render and turning routine "find a note" loops into ~170 KB-per-call
scans.

## Bead(s)

- `bd-eb1b56` — `caco scratch list --json` inlines full `content`
  field for every note (P3 bug, test-user surfaced)

## Before state

- Failing tests: none related
- `caco scratch list --json | wc -c` ≈ 173 554 (56 notes)
- `caco scratch list | wc -c` ≈ 4 327 (text mode, same 56 notes)
- `data[0]` keys included a full `content` string for every list item
- No `--include-content`, `--content-preview`, or `content_len`
  affordances existed on either daemon or CLI

## After state

- Failing tests: none related (162-test small preflight green;
  3 new integration tests + 7 new unit tests added)
- Default `caco scratch list --json`: omits `content` entirely;
  always emits `content_len` so byte-size affordances still render
  cheaply; emits `content_truncated: true` only when a preview
  actually dropped bytes
- `caco scratch list --json --include-content` → legacy full body
- `caco scratch list --json --content-preview 200` → first 200 bytes
  per note, UTF-8 char-boundary safe (multi-byte cuts back off to
  the previous codepoint)
- Daemon endpoint matches: `?include_content=true` and
  `?content_preview=N` query params on `GET /api/v1/scratchpads`
- TUI `ScratchpadNoteInfo` deserializer is forward-compat (`content`
  is `serde(default)`; new fields wired) so older daemons keep working
- TUI agent-filter list call explicitly opts into `?include_content=true`
  so existing scratch-tab rendering is unchanged

## Diff summary

- Commit: `eb22f4add` on agent branch
- Files touched:
  - `crates/caco-daemon/src/lib.rs` (+ `ScratchpadListItem` projection,
    `floor_char_boundary` helper, query-param plumbing, 7 unit tests)
  - `crates/caco-daemon/tests/daemon.rs` (+3 integration tests for
    default omission, `include_content=true`, `content_preview=N`)
  - `crates/caco-cli/src/lib.rs` (+2 args, dispatch plumbing,
    `content_len`-aware text rendering)
  - `crates/caco-tui/src/client.rs` (forward-compat
    `ScratchpadNoteInfo`, explicit `include_content=true` opt-in
    on the existing filtered-list helper)
  - `crates/caco-tui/src/views/fuzzy_picker.rs` (test fixture
    constructor updated for new fields)
- Tests: +10 / -0 / flipped 0
- Behavioural delta: list-mode JSON shrinks from O(notes × body) to
  O(notes × constant) by default; opt-in flags preserve every prior
  use case

## Operator-takeaway

`caco scratch list --json` is now a metadata-only enumerator by
default — the right shape for "find me a note" scripts and the right
shape to align with `bd list` / `msg list` patterns. Any caller that
genuinely needs bodies must say so via `--include-content` or
`--content-preview N`. Future scratchpad surfaces should follow the
same pattern: list endpoints return projections, full content lives
behind `show`.
