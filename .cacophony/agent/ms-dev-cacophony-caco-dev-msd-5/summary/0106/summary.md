# Session summary — caco release trigger silent no-op → error (bd-77cb8d)

## Goal

`caco release trigger --channel <X> --project <P>` for a project whose channel
does not resolve returned a completed exit-0 job in ~3ms that built and
dispatched nothing — a dangerous false-success on a release surface (an operator
or agent believes a release was cut when nothing happened).

## Bead

- `bd-77cb8d` (P2 bug) — Part 1 (harness safety). Part 2 (config-merge drift:
  per-project repo `.cacophony/config.yaml` `releases:` showing null in effective
  daemon config) is separate — follow-up to be filed.

## Root cause

In `handle_release_trigger` (crates/caco-daemon/src/lib.rs), `strategy` was
resolved as `body.strategy.or_else(|| channel_config.map(|c| c.strategy)).
unwrap_or(ReleaseStrategy::Node)`. So when the requested channel did not resolve
(no matching channel, or releases config absent) AND no explicit strategy was
supplied, it silently defaulted to `Node`, which — lacking any channel
build_command/push_command/workflow — produced a completed exit-0 no-op job.

## After state

- Added a harness-safety guard via a pure, unit-tested helper
  `release_trigger_rejection_message(channel_resolved, strategy_explicit,
  release_config_present, channel, project) -> Option<String>`: reject with
  `400 no_release_channel` and a clear message when the channel does not resolve
  and no explicit strategy was supplied, distinguishing "no release config" from
  "channel not found". An explicit strategy is preserved (caller's deliberate
  choice).

## Diff summary

- Code commit: `bd-77cb8d: error instead of silent no-op when caco release
  trigger channel does not resolve` (final landed squash SHA per the
  reintegration receipt).
- File: `crates/caco-daemon/src/lib.rs` (guard + `release_trigger_rejection_message`
  helper + unit test).
- Test: `release_trigger_rejection_message_guards_silent_noop_bd_77cb8d`
  (real-cargo validated green via queued job tj-09d8281d, exit 0).

## Operator-takeaway

`caco release trigger` now errors clearly instead of a silent exit-0 success when
the channel doesn't resolve, closing the false-success footgun. Part 2 (why a
project's repo releases config shows null in the daemon's effective config — a
config-merge/loading question) is a separate follow-up.
