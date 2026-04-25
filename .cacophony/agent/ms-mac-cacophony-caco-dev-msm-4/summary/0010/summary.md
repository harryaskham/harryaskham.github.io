# Slice 8 — bd-9518e2: canonical error for invalid tts io output set --mode

## Goal

Fix misleading error when `caco tts io output set --mode ''` or `--mode bogus` falls through to "named pulse output requires --server" instead of enumerating valid modes.

## Bead(s)

- **bd-9518e2** (bug, P3) — caco tts io output set --mode bogus emits misleading error.

## Before state

- `caco tts io output set --mode ''` → `named pulse output '' requires --server address`
- `caco tts io output set --mode bogus` → `named pulse output 'bogus' requires --server address`
- User thinks they need `--server` when they actually need a valid mode.

## After state

- Empty mode: `--mode value cannot be empty for tts io output set (allowed base modes: local-default, local-device, pulse-default; or a named pulse output with --server).`
- Unrecognized mode: `--mode 'bogus' is not a recognized base mode (allowed: local-default, local-device, pulse-default). If 'bogus' is a named pulse output, also pass --server <host:port>.`
- Matches fleet canonical pattern from bd-2b096e, bd-58220a, caco notify list --level.

## Diff summary

```
 crates/caco-cli/src/lib.rs | 12 +++++++++++-
 1 file changed, 11 insertions(+), 1 deletion(-)
```

## Operator-takeaway

Invalid `--mode` values in `caco tts io output set` now show which modes are valid instead of suggesting the wrong flag.
