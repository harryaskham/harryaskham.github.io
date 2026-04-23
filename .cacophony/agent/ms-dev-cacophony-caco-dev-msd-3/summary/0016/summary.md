# Session summary — bd-46a8f3: walk error.source() chain in image_api_generate

## Goal

Operator's `caco image generate` command was returning the opaque
message `error: image generation request failed (POST <url>):
builder error` with no actionable detail — operator workaround
was falling back to direct TTI invocation. Surface the underlying
cause so the failure is self-diagnosing.

## Bead(s)

- `bd-46a8f3` — caco image generate fails with opaque 'builder error'
  for all presets

## Before state

`image_api_generate`'s `.map_err` on the reqwest `.send()` call
formatted the error with `format!("...: {e}")`. reqwest's `Error`
type's `Display` impl only shows the kind (`"builder error"`,
`"connect error"`, etc.) and hides the actual source (URL parse,
malformed header, DNS failure). The URL was already surfaced in
the message (bd-2f7850, prior fix), but the *cause* of the
build/connect failure wasn't.

## After state

`map_err` now walks `std::error::Error::source()` in a `while let
Some(s) = src` loop, joining each layer with `": "`. Operator
now sees the full chain, e.g.

    error: image generation request failed (POST <url>): builder \
    error: relative URL without a base

— immediately diagnosable as a missing `https://` scheme on the
`openai_base_url` template (the suspected root cause per the bead's
"asks" section).

## Diff summary

- `crates/caco-cli/src/lib.rs` (+40 / -5):
  - `image_api_generate` send-error map closure walks source chain.
  - New test `image_api_generate_error_walks_source_chain` pickaxe-
    greps the source for the `source()` / while-let-Some pattern so
    a future simplification of the error mapping can't silently
    regress.
- Existing test `image_api_generate_error_surfaces_url` unaffected.
- `cargo test -p caco-cli --lib image_api_generate`: 2/2 pass.
- `cargo build -p caco-cli` clean.

## Embedded artefacts

(none)

## Operator-takeaway

This is a one-line behavioural fix wrapped around a chain-walking
idiom that should probably be applied to every other `reqwest`
error-mapping site in the codebase (probably 20+). Filing a
sibling for that sweep is left for another agent / cycle. The
bead's secondary "asks" (verify openai_base_url template renders
correctly; confirm gpt-image-2 / 3840x2160 / quality:high pass
the validators) are upstream of this fix and can now be
diagnosed by simply re-running the operator's repro and reading
the now-meaningful error chain.
