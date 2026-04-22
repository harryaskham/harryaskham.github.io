# Session 0004 — bd-de2acd

## Goal

Stop the caco-web Logs view from accumulating duplicate copies of the daemon
log tail on every transient EventSource reconnect (tail-replay storm).

## Bead(s)

- bd-de2acd — claimed and worked end-to-end.

## Before state

- `crates/caco-web/static/app.js` opened `/api/v1/logs/stream` without
  `?follow=true`. Server returned the tail and closed the stream with
  `event: done`. EventSource auto-recovery scheduled `startLogStream()`
  every 5s, which re-fetched the same tail and `state.logs.push()` blindly
  re-appended every line. After ~5 min idle the in-browser view contained
  the same handful of error lines repeated dozens of times.
- No client-side dedup. No stable `id:` on the server. No `Last-Event-ID`
  honouring on reconnect.

## After state

Three independent fixes (all landed):

- **Frontend** (`crates/caco-web/static/app.js`):
  - `EventSource('/api/v1/logs/stream?follow=true')` — stream stays open,
    server polls every 500ms emitting only new lines.
  - New `logSeenKeys: Set<string>` defensive dedup, keyed by full line and
    bounded by the same 3000-entry rolling window as `state.logs`. When an
    old line ages out of the window the next identical occurrence is
    accepted as new.

- **Server** (`crates/caco-daemon/src/lib.rs handle_logs_stream`):
  - Every emitted SSE frame now begins with `id: <offset>` where `<offset>`
    is the 1-based line number in daemon.log. Browsers expose the most
    recent id via `Last-Event-ID` on EventSource auto-reconnect.
  - Follow-mode handler now reads `Last-Event-ID` from the request headers
    and skips initial-tail lines whose offset is already covered. Reconnects
    resume from the next line instead of re-delivering the tail.
  - Non-follow mode also emits `id:` per frame so callers can switch modes
    without losing position.

- **Tests**:
  - New `logs_stream_non_follow_includes_stable_id_per_line` locks the SSE
    wire format (every `data:` frame preceded by `id: <number>`).
  - Existing `logs_stream_non_follow_returns_sse` and
    `logs_stream_follow_tail_zero_has_no_initial_replay` continue to pass.

## Diff summary

```
crates/caco-daemon/src/lib.rs     | ~80 +/-
crates/caco-web/static/app.js     | ~26 +/-
.cacophony/agent/.../summary/0004 | (new)
```

Two files touched (server + frontend) plus this summary. Net additive
(~98 lines added across 2 files).

## Validation

- `cargo test -p caco-daemon --lib -- logs_stream --test-threads=1` → 3/3 PASS
- `cargo clippy --workspace --all-targets -- -D warnings` → clean
- `cargo test-small` → final binary 52/52, full sweep clean

## Operator-takeaway

The Logs view in caco-web should stop accumulating duplicates immediately
after this lands. If transient connection drops still happen (network blips,
daemon restarts), the client now resumes cleanly from the last delivered
line via `Last-Event-ID` rather than replaying the tail.

The same anti-pattern (EventSource + non-follow default + auto-reconnect +
push-without-dedupe) may exist on other views; bd-a7168d tracks the visual
pop-in cousin. Worth a sweep after this change has soaked.

## Coordination

- Spoke claim of bd-de2acd before starting.
- Will speak completion + reintegrate before picking next bead.
