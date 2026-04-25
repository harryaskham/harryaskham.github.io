# Slice 10 — bd-377c4b: wrap tts status / io output show --json in standard envelope

## Goal

Bring `caco tts status --json` and `caco tts io output show --json` into the canonical `{ok, data, meta}` envelope established by bd-bbcc36 / bd-548e77 / bd-3e39a0 so scripts can `jq -e .ok` consistently.

## Bead(s)

- **bd-377c4b** (bug, P3) — bare-flat-object JSON envelope drift in tts namespace.

## Before state

```
$ caco tts status --json | jq 'keys'
["last_speak_at","model","muted","ok",...]   # bare flat with peer 'ok'

$ caco tts io output show --json | jq 'keys'
["mode","ok","pulse_server","source"]         # bare flat with peer 'ok'
```

Sister `caco audio capabilities --json` was already canonical `{ok,data,meta}`. Three distinct shapes within audio/tts/status.

## After state

Both surfaces now emit:

```json
{"ok": true, "data": {...full payload...}, "meta": {}}
```

## Diff summary

```
 crates/caco-cli/src/lib.rs | 26 ++++++++++++++++++++++++--
 1 file changed, 24 insertions(+), 2 deletions(-)
```

Two `if json_requested` branches in `dispatch_tts_control` (status path) and `dispatch_tts_io_output_show` now wrap `response` as `data` rather than serializing it bare. `cargo check -p caco-cli` clean.

## Operator-takeaway

Two more JSON envelope shapes collapsed into the canonical `{ok,data,meta}` family. Bare-object catalog reduced.
