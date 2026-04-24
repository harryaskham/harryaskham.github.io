# Session summary — bd-b6a0d9: caco event log --since '-1h' now reaches semantic validator

## Goal

Two-part fix for `caco event log --since -1h` and the
`--since` cluster:

1. **Parser-side relaxation**: extend
   `is_numeric_value_token` (bd-8b4559 helper) to accept
   duration-suffixed numeric tokens (`-1h`, `-30m`,
   `-1.5d`, `-15s`). Without this, the unsupported-flag
   branch eats `-1h` before the per-surface validator
   ever sees it — leaving the operator with the
   misleading `error: unsupported flag: -1h` even though
   `-1h` is lexically a duration.

2. **Semantic-side rejection**: `parse_since_duration`
   explicitly rejects negative durations with a clear
   error suggesting the positive form, instead of
   silently routing through to a downstream
   `chrono::Duration` of negative seconds (which would
   then make `now - dur` move into the future).

This is option (a) + (c) from the bead's recommendation
(skipping (b) "document the inline `--since=-1h`
workaround" because the parser+validator combo now
solves it without needing operator-side workarounds).

## Bead(s)

- `bd-b6a0d9` — `caco event log --since '-1h' rejected
  because parser requires positive duration; bd-8b4559
  numeric-token helper rejects '-1h' (not pure numeric)`.

## Before state

- `caco event log --since '-1h'` →
  `error: unsupported flag: -1h`
- `caco event log --since=-1h` would technically have
  worked at the parser level but then silently accepted
  the negative duration (computing `now + 1h` as the
  cutoff), giving zero results without saying why.

## After state

- `caco event log --since -1h` →
  `error: invalid --since value: -1h (durations must be
  positive; use --since 1h for 1h ago)`
- `caco summary --since -1h` (and any other surface
  using `parse_since_duration`) → identical error.
- `caco event log --since 1h` (the canonical positive
  form) → unchanged, works as before.
- `caco tts set --speed -1` (the original bd-8b4559
  exemplar) → unchanged, the parser still routes `-1`
  to `--speed`.

## Diff summary

- `crates/caco-cli/src/lib.rs`:
  - `is_numeric_value_token`: after extracting `body`
    (sign-stripped), if the last char is in `{s,m,h,d}`
    and there's something before it, strip that suffix
    before doing the digit/dot scan. Accepts `-1h`,
    `-30m`, `-1.5d`, `-15s`, `1h`, `30m`, `1d`, `5s`.
    Still rejects bare `-h`, `-1abc`, `1h2`, etc.
  - `parse_since_duration`: after parsing `num`, reject
    `num < 0` with a USAGE-GUIDANCE error that suggests
    the positive form (computed `-num` so the suggestion
    is concrete: `--since 1h for 1h ago`, not generic
    "use a positive duration").
  - Extended `bd_8b4559_is_numeric_value_token_classification`
    accept-list with the 8 new duration-suffixed cases
    so future refactors can't regress.
  - New test
    `parse_since_duration_rejects_negative_with_positive_suggestion`
    asserts both the "durations must be positive" wording
    and the "--since 1h" suggestion substring.
- `cargo test -p caco-cli --lib parse_since_duration` +
  `bd_8b4559_is_numeric_value_token_classification`: all 11
  tests pass.
- `cargo test-small`: 297 pass; 1 pre-existing failure in
  `caco-profile::shipped_profiles_html_lists_every_canonical_profile`
  (someone added `.cacophony/profiles/stale-check.md`
  without updating `docs/profiles.html` — verified
  pre-existing by stashing my changes and re-running).
  Not my work; logged for follow-up.

## Operator-takeaway

The `--since` cluster is now defensively correct end to end:

- **Parser layer**: any well-formed numeric token (with or
  without sign, with or without duration suffix) reaches
  the per-surface validator instead of being eaten by the
  unsupported-flag branch.
- **Semantic layer**: `parse_since_duration` rejects
  negative durations with USAGE-GUIDANCE ("use --since
  1h for 1h ago") so the operator gets a turnaround
  rather than a confusing flag-parser error.

The `parse_since_duration`-based surfaces (caco summary,
caco event log, caco changelog show, etc.) inherit this
fix for free.

The bd-8b4559 widening is bounded: `s|m|h|d` are the only
suffixes recognised, and the suffix is only stripped if
non-empty content precedes it. The other negative-flag
recipients (--speed, --offset, --depth) still work — `-1`,
`-1.5`, `-.5` go through the original sign+digit path
unchanged.

The pre-existing `caco-profile` test failure
(`stale-check` profile not listed in docs/profiles.html)
is worth a quick follow-up bead — likely a missed
update from the operator-side stale-check rollout.
