# Session summary — bd-816c9a GPT-Image-2 support

## Goal

Add gpt-image-2 model knobs (--quality enum + --size with 3840x2160
ceiling) to caco image generate.

## Bead(s)

- `bd-816c9a` — Add GPT-Image-2 model support

## Before state

- dispatch_image_generate hardcoded 1024x1024 cap from model.size only
- No --quality flag; quality field never reached the API
- No size validation on the gpt-image-* family

## After state

- --quality {high,medium,low} CLI flag, default high
- --size WxH CLI flag with per-family upper-bound validation
- Cascade: cli > preset > model config > default
- Quality forwarded into request payload only for gpt-image-* models
  (other models would reject the unknown field)
- ImageModelConfig + ImagePresetConfig gain optional quality/size
  fields with compile-time schema exhaustiveness updated

## Diff summary

- Commits: d6b6e525d3aa
- Files: `crates/caco-cli/src/lib.rs` (+148 -7),
  `crates/caco-config/src/model.rs` (+27 -2),
  `crates/caco-daemon/src/timeline.rs` (broken-on-main clippy fix)
- Tests: +3

## Operator-takeaway

Did NOT silently flip the default model to gpt-image-2 \u2014 the
operator config is the source of truth and the CLI cascade picks
the first configured model. To make gpt-image-2 the default,
reorder `images.models` in cacophony.toml. If you want the CLI
to hardcode a default model name, file a follow-up.
