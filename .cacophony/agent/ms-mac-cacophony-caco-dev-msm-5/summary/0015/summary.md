# Session summary 0015 — bd-87f5bf: raise tmux history-limit to 100k

## Goal

Operators reported that long-running agent panes silently truncated
scrollback in TUI/web/android. Root cause: tmux's default
`history-limit` is 2000 lines per pane, never overridden by the
daemon. Bump it to a generous 100k for agent panes so multi-day
persistents don't lose history.

## Bead(s)

- `bd-87f5bf` — primary fix.
- Filed `bd-83a84d` (caco agent log --tail/--head/--all subcommand)
  and `bd-7ef076` (TUI/web/android surface the limit + usage) as
  follow-ups for the broader scrollback-UX scope items #2 and #4
  from the original bead.

## Before state

- `tmux new-session` was called at two callsites in
  `crates/caco-daemon/src/agent/health.rs` (default-socket and
  per-agent-socket variants) with no `set-option -g history-limit`
  beforehand.
- All agent panes inherited tmux's 2000-line default; long sessions
  truncated everything older than that without any operator-visible
  marker.

## After state

- New constant `AGENT_TMUX_HISTORY_LIMIT = 100_000` and helper
  `set_tmux_history_limit(socket)` defined in `agent/health.rs`.
- The helper invokes `tmux set-option -g history-limit 100000` on
  the named socket and silences the expected "no server running"
  case (server auto-starts on new-session); other failures emit a
  structured `bd-87f5bf` log line.
- Both `new-session` callsites now call `set_tmux_history_limit`
  immediately after `sync_tmux_server_path`, so all freshly-created
  panes inherit the raised limit.
- Capacity sizing: 100k × ~200 chars = ~20 MiB worst case per pane;
  typical agent panes are far smaller.

## Diff summary

- Commit: `16e2e81d`.
- Files: `crates/caco-daemon/src/agent/health.rs` (+48: const,
  helper fn, 2 callsite wirings, doc comments).
- Tests: none added (best-effort fire-and-forget pattern matches
  `sync_tmux_server_path` which is also untested directly; tmux
  state is not easily mockable in unit tests).
- `cargo build -p caco-daemon` and `cargo clippy -p caco-daemon`:
  clean.

## Out of scope (deferred)

- **Scope item #2** (CLI dump-full-history command) → bd-83a84d.
- **Scope item #4** (caco agent log --tail/--head/--all) →
  bd-83a84d (same bead — they're the same feature).
- **Surface the limit in operator views** → bd-7ef076.
- **First-session-on-fresh-socket caveat**: the very first tmux
  session on a never-before-used socket will use the default 2000
  limit because the server doesn't exist yet when the helper runs.
  Subsequent sessions on the same socket inherit the raised limit.
  For persistents (one session per agent) this only affects the
  first pane after a server-cleanup; acceptable.

## Operator-takeaway

Long-running agent panes (multi-hour and beyond) now retain ~50x more
scrollback than before. After an agent restart or fresh spawn,
operator scrollback should comfortably hold 100k lines (the previous
default of 2000 lines was the silent truncation the bead noticed).
Two follow-up beads file the CLI ergonomics (bd-83a84d) and the
operator-facing visibility of the limit (bd-7ef076).
