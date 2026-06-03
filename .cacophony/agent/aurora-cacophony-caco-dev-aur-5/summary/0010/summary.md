# Session Summary — bd-2696dd (TUI config-watcher idle CPU peg)

## Bead
**bd-2696dd** (P1 bug) — *caco tui pegs a CPU core when idle: config watcher floods on canonical-checkout churn (full-ancestor readlink loop in notify debouncer).*

## Problem
A static/idle `caco tui` burned ~100-114% of one CPU core indefinitely. Traced to the `notify` debouncer thread in `crates/caco-tui/src/config_watcher.rs` (bd-12d412 live config reload): `strace -c` showed ~14k failing `readlink`/sec re-canonicalizing the full ancestor path of every watched config file on every inotify event.

Root cause: `watch_targets` watches the **parent directory** (NonRecursive) of each config file. On canonical-daemon-checkout-backed configs those dirs (`~/.cacophony`, `daemon/checkouts/<project>/.cacophony`, `themes/`, `agents/`) are rewritten on **every** config distribution and **every** git update to the daemon checkout. The event handler reacted to **any** change event and then re-canonicalized the whole import graph per event, so continuous reintegration traffic (authority node, ms-mac ~30 agents) pegged a core continuously. This load directly aggravated the beads-handler CPU-starvation wedges seen repeatedly this session.

## Fix
Applied bead fix-direction #1 (lowest-risk, highest-leverage): **basename allow-list filtering at the source.**
- New `watched_config_file_names()` computes the set of config-file basenames from the watched import graph at startup.
- New `events_touch_watched_config()` drops every debounced batch whose paths don't include a known config-file basename, **before** any rematerialize/canonicalize work runs.
- `make_event_handler` / `start_backend` / `start_config_watcher` thread an `Arc<HashSet<OsString>>` of known names through both native and poll backends.
- Empty known-name set (bare runtime-dir fallback when the config graph can't resolve at startup) preserves prior permissive behaviour so config creation is still detected.
- Also breaks the `IN_ATTRIB` read→atime→event feedback risk in practice, since unrelated atime events are now filtered out.

The expensive `rematerialize_config` path (which drives the readlink flood) now only runs on genuine config-file changes, so an idle TUI stays near 0% CPU.

## Validation
- Queued daemon test run: `cargo test -p caco-tui --lib config_watcher` → **14 passed, 0 failed** (8 pre-existing + 6 new).
- New tests: basename collection (dedup), known-file matching across watched dirs, unrelated-churn rejection (.git/index.lock, agent summaries, .tmp), mixed real-change-among-noise batch, empty-known-set permissive fallback, empty batch rejection.
- merge-queue `before_reintegration` gate (test-small + `cargo check --workspace --tests` + clippy) runs at reintegration.

## Scope
Single file: `crates/caco-tui/src/config_watcher.rs` (+161/-8). No public API change; no config schema change. Behaviour change is strictly a reduction in spurious work.

## Diff
Code landed in commit **ad99c55445** (`bd-2696dd: filter TUI config-watcher events by basename to stop idle CPU peg`). Final landed squash SHA: see reintegration receipt.

## Notes / Context
This bead was the traced root cause of the runaway-`caco tui` CPU saturation that contributed to today's repeated helsinki beads-primary CPU-starvation wedges. Other listed fix directions (#2 watch specific files, #3 rate-limit/circuit-break, #4 canonicalize-once, #5 drop IN_ATTRIB, #6 don't watch the daemon-checkout config dir) remain available as follow-ups if basename filtering proves insufficient on extreme nodes, but #1 removes the per-event flood cost that caused the peg.
