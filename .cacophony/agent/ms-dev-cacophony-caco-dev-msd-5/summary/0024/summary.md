# Session summary — Pi oversized-image guard

## Goal

Make managed Pi sessions resilient when a screenshot or other image is too large for the provider request, avoiding the 413 payload-too-large failure mode that can strand the agent after an image read.

## Bead(s)

- `bd-72e053` — Pi: recover gracefully from 413 payload-too-large after image reads

## Before state

- Failing tests: none known for this scope.
- Relevant metrics: Pi's upstream read tool already resizes images toward a larger inline limit, but the operator observed a real 413 after an oversized screenshot workflow; this repo had no managed-profile guard that returned metadata-only recovery before provider payload construction.
- Context: managed Pi workers compose repo-owned overlays for loop, self-compact, self-nudge, and Tendril, but large screenshot handling relied on the default read tool behavior.

## After state

- Failing tests: none observed in targeted validation.
- Relevant metrics: `node .cacophony/pi/image-guard/extensions/caco-image-guard.test.mjs` passed; `node --check` passed for the image-guard extension and utility module; `cargo check -p caco-cli --tests` passed; `cargo run -q -p caco -- config validate --config .cacophony/config.yaml` passed with only pre-existing warnings.
- Context: managed project defaults now compose `pi-image-guard`, which wraps Pi's `read` tool and enforces a 1,000,000-byte image budget by default before full image attachments enter provider context.

## Diff summary

- Commits: current HEAD for this summary chunk (`bd-72e053: add Pi oversized-image guard`)
- Files touched: `.cacophony/pi/image-guard/extensions/caco-image-guard.mjs`, `.cacophony/pi/image-guard/extensions/caco-image-guard-utils.mjs`, `.cacophony/pi/image-guard/extensions/caco-image-guard.test.mjs`, `.cacophony/profiles/pi-image-guard.md`, `.cacophony/project.yaml`, `README.md`, `AGENTS.md`, `SPEC.md`, `.cacophony/agent/ms-dev-cacophony-caco-dev-msd-5/summary/0024/summary.md`
- Tests: +1 Node utility test file / -0 / flipped 0
- Behavioural delta: large `png`, `jpg`, `jpeg`, `gif`, and `webp` reads in managed Pi sessions now either attach a bounded downscaled preview or return concise metadata and recovery guidance instead of sending the original full-resolution image attachment.

## Operator-takeaway

The next oversized screenshot should not take the Pi session down with a provider 413. The agent will see a clear warning, path/size metadata, and possibly a smaller preview, preserving working state so it can crop, downscale, or inspect metadata deliberately.
