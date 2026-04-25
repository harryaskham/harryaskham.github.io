# Session summary — msg broadcast now rejects empty bodies up front

## Goal

Land a small mainline validation fix after `test-user-hel` disclosed an
accidental empty-body project broadcast (`caco msg broadcast --body ""`). The
goal was to stop empty or whitespace-only broadcast bodies before they reach the
daemon, matching the stricter behavior already present on sibling surfaces like
`msg speak` and `caco loop`.

## Bead(s)

- `bd-0a433f` — Reject empty msg broadcast bodies before daemon round-trip

## Before state

- `msg broadcast` required the `--body` flag, but did not reject empty or
  whitespace-only values.
- That meant an accidental `caco msg broadcast --body ""` could still make it
  through the CLI and hit the daemon as a real broadcast attempt.
- `msg speak` already rejected empty bodies client-side, and `caco loop` had the
  same style of guard, so `msg broadcast` was inconsistent with sibling
  messaging surfaces.

## After state

- `caco msg broadcast` now rejects empty / whitespace-only `--body` values
  before any daemon round-trip with:
  - `--body must not be empty for msg broadcast`
- Added a focused regression test covering:
  - empty string body
  - spaces-only body
  - newline / tab whitespace-only body
- This brings `msg broadcast` into parity with the existing empty-body guards on
  adjacent messaging / looping surfaces.

## Diff summary

- Files touched:
  - `crates/caco-cli/src/lib.rs`
- Tests added:
  - `msg_broadcast_rejects_empty_or_whitespace_body`
- Validation:
  - `cargo fmt --all`
  - `cargo test -p caco-cli msg_broadcast_rejects_empty_or_whitespace_body -- --nocapture`
  - `cargo build -p caco-cli`
- Behavioural delta:
  - accidental empty-body broadcasts are stopped immediately in the CLI instead
    of becoming daemon-visible noise

## Operator-takeaway

This is a small but worthwhile messaging-safety parity fix. A user can still
omit `--body` and get the existing required-flag error, but they can no longer
accidentally send an empty or whitespace-only broadcast just because the flag
was present. That keeps project chat cleaner and aligns `msg broadcast` with the
more careful validation already used by related surfaces.
