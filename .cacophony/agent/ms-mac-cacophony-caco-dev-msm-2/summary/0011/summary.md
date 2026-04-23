# Session summary — `caco tts set --filter` (bd-a96ff3)

## Goal

Extend the unified `caco tts set` setter with a `--filter` arg
so operators can flip the voice filter atomically alongside
voice/model/speed changes, matching the `set-*` parity intent
and avoiding a second round-trip via `caco tts filter on|off`.

## Bead(s)

- `bd-a96ff3` — Add new filter args to caco tts set command
  (P2, caco, cli, filters, tts)

## Before state

- `caco tts set` accepted `--voice`, `--model`, `--speed`.
- `caco tts filter on|off` was a separate runtime mutation,
  forcing two RPCs (and two persistence writes) to flip both
  voice + filter atomically.
- `TtsSetRequest` didn't have a `filter` field; the daemon
  endpoint silently ignored anything unknown, so retrofit
  needed both ends touched.

## After state

- `TTS_SET_ARGS` advertises `--filter` (on|off|true|false
  vocabulary).
- `dispatch_tts_set` parses `--filter` via new helper
  `parse_tts_filter_flag` and packs `{filter: bool}` into the
  POST body when present.
- `TtsSetRequest` gains `filter: Option<bool>`.
- `handle_tts_ctrl_set` mutates `rt.filter_enabled` when
  `body.filter` is present — single mutex acquisition, single
  `persist_tts_daemon_state` call (atomic with the existing
  voice/model/speed mutation).
- Human-readable summary surfaces `filter: on|off` alongside
  the other resolved fields.
- Empty-payload guard updated: at least one of
  `--voice/--model/--speed/--filter` required (was three).

## Diff summary

- Files touched:
  - `crates/caco-cli/src/lib.rs` — TTS_SET_ARGS, TtsSetRequest,
    handle_tts_ctrl_set, dispatch_tts_set, parse_tts_filter_flag
    helper, dispatcher branch, 3 new tests
- Tests: +3 / -0 / flipped 0
  - `parse_tts_filter_flag_accepts_synonyms` — 8 truthy + 8
    falsy + whitespace-tolerance
  - `parse_tts_filter_flag_rejects_garbage` — empty / "maybe" /
    "truthy" all error with helpful "must be one of" hint
  - `tts_set_args_advertises_filter_flag` — pins ArgSpec
    registration so help listing surfaces `--filter` and a
    future dispatcher-collapse can't silently drop it
- Test command:
  `cargo test -p caco-cli parse_tts_filter`
  `cargo test -p caco-cli tts_set`
  → 4 passed, 0 failed.
- Build verified clean: `cargo build -p caco-cli`.

## Operator-takeaway

You can now collapse voice + filter changes into one call:

```
# Before: two calls, two persistence writes.
caco tts set --voice Leda
caco tts filter on

# After: one atomic call.
caco tts set --voice Leda --filter on
```

Vocabulary matches `caco tts filter on|off` exactly:
`on/off/true/false/1/0/enabled/disabled/yes/no` (case-
insensitive, whitespace-tolerant). Garbage values get a
"--filter must be one of …" error with the offending value
echoed back.

`caco tts filter on|off` remains as a single-purpose shortcut
(not deprecated). bd-205b39 (sibling: align `tts set` with
`set-*` patterns more broadly) is a follow-up — this bead
addresses the `--filter` argument specifically. If the operator
wants additional filter-related args (preset selection,
filter-strength curves, etc.) those would extend `TTS_SET_ARGS`
the same way.

Honored constraints:
- No `cargo test --workspace`; targeted to two filter strings
  (4 tests run).
- No daemon/sidecar surface touched outside the local-TTS
  daemon (`handle_tts_ctrl_set` lives in `caco-cli/src/lib.rs`
  alongside the other `handle_tts_ctrl_*` siblings).
- Operator no-narrator rule honored — claim + close speaks
  issued by msm-2 directly.

14th bead closed this session (cumulative).
