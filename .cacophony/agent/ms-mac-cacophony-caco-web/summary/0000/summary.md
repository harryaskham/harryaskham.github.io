# Session summary — caco-web lightweight Playwright profile update

## Goal

Update the persistent caco-web worker profile so future dashboard observation cycles use the lightweight `playwright-cli` path Harry requested, rather than reaching for the heavier `npx playwright` package or full browser install flow by default.

## Bead(s)

- `bd-319bfe` — Document lightweight caco-web playwright-cli workflow

## Before state

- Failing tests: none known for this profile-only change.
- Relevant metrics: `which playwright-cli` returned not found; `npm view @playwright/cli` showed version `0.1.9` with a `playwright-cli` binary; `npx playwright screenshot` attempted to use a missing Playwright browser cache and recommended a full browser install.
- Context: the caco-web profile only said to use `playwright-cli` and file a setup bead if missing, but did not document the working npm package, the lighter session commands, or the macOS/nix-shell socket-path workaround.

## After state

- Failing tests: none known.
- Relevant metrics: `TMPDIR=/tmp npx --yes @playwright/cli --version` returned `0.1.9`; `TMPDIR=/tmp npx --yes @playwright/cli -s=caco-web snapshot --raw` captured a 731-line dashboard snapshot; `caco profile show --name caco-web --json` parsed successfully; `git diff --check -- .cacophony/profiles/caco-web.md` passed.
- Context: the profile now prefers a PATH `playwright-cli` when available, documents the `@playwright/cli` fallback, sets `TMPDIR=/tmp` for macOS/nix-shell, fixes the local `caco web` launch flags, and tells workers to avoid full browser downloads unless the lightweight CLI says they are actually required.

## Diff summary

- Commits: `93070963b`
- Files touched: `.cacophony/profiles/caco-web.md`
- Tests: +0 / -0 / flipped 0
- Behavioural delta: Future caco-web agents have a concrete, validated lightweight browser-observation recipe and a documented workaround for Playwright daemon socket failures under long nix-shell temp paths.

## Operator-takeaway

The caco-web profile now encodes the exact lightweight Playwright workflow Harry asked for, including the package name and `TMPDIR=/tmp` gotcha discovered in-session, so future persistent caco-web loops should stop defaulting to heavyweight Playwright installs.
