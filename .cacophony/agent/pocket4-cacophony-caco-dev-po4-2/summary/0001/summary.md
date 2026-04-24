# Session summary — `caco tmux status` shows every caco-* socket; `tmux send` mutex enforced

## Goal

Stop `caco tmux status` from lying about node tmux state. Pre-fix it
was hardcoded to `caco-test-*`, so a node hosting nine persistent
agent sockets reported `0 sockets`. Same surface: tighten the
`tmux send` flag-mutex story (4th instance in the bd-a97ad4 family —
`--socket` and `--agent` are documented as alternatives but were
silently dropped if both supplied).

## Bead(s)

- `bd-f3d81e` — caco tmux status / cleanup hardcoded to `caco-test-*`
  prefix; tmux send `--socket`+`--agent` mutual-exclusion miss (P3 bug,
  test-user surfaced)

## Before state

- `caco tmux status` on a node with 10+ live `caco-agent-*` sockets
  printed `0 caco-test-* socket(s)` and "No caco-test-* tmux sockets
  found." Operators read this as "no caco tmux state" — false.
- `caco tmux send --target … --keys 'x' --socket s1 --agent a1`
  silently selected `s1`, dropped `a1`, then errored on the wrong
  socket. No mutex error.
- Failing tests: none related.

## After state

- `caco tmux status` default now segments every `caco-*` socket into
  Test / Agent / Other sections with per-section emptiness hints and
  a heading like `caco tmux status — 11 caco-* socket(s) (1 test,
  10 agent, 0 other)`.
- `--test-only` flag preserves the legacy filter and heading for any
  scripts greping for `caco-test-* socket(s)`.
- JSON mode adds `counts: {test, agent, other}` and a `test_only`
  marker alongside the flat `sockets` array.
- `caco tmux send` rejects `--socket` + `--agent` together with a
  friendly mutex error naming both flags.
- 4 new tests (2 daemon-side classifier + live-discovery, 2 CLI-side
  mutex + required-flag regression). 162-test small-preflight green.

## Diff summary

- Commit on agent branch (pre-reintegration); replays cleanly on
  current main.
- Files touched:
  - `crates/caco-daemon/src/agent/health.rs` (+
    `CacoTmuxSocketKind`, `CacoTmuxSocketInfo`, `list_caco_tmux_sockets`)
  - `crates/caco-daemon/src/agent/tests.rs` (+2 tests)
  - `crates/caco-cli/src/lib.rs` (`TMUX_STATUS_ARGS`, status spec
    update, `dispatch_tmux_status` rewrite, render helpers,
    `dispatch_tmux_send` mutex guard, +2 unit tests)
- Tests: +4 / -0 / flipped 0
- Behavioural delta: `caco tmux status` no longer hides agent
  sockets; `caco tmux send` no longer silently picks one of two
  conflicting socket sources.

## Operator-takeaway

Two long-standing hidden defaults fixed in one bead:
1. `caco tmux status` is now an honest answer to "what tmux state
   does caco own on this node?" — the answer used to be "only the
   transient test ones I happen to know about." Add `--test-only`
   when scripting against the legacy heading.
2. The mutual-exclusion-validator gap from bd-a97ad4 / bd-2c88ed /
   bd-33b6d9 is now closed for `caco tmux send` as well. The
   centralised mode-flag-validator helper proposed in bd-a97ad4 is
   still the right long-term fix; this bead is the targeted
   per-surface guard ahead of that landing.
