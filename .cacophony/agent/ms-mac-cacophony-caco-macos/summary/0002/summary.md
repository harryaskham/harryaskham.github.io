# Session summary — libghostty supply-chain recommendation

## Goal

Validate the libghostty / GhosttyKit supply-chain path before Cacophony adds any Ghostty-backed terminal renderer dependency to the production macOS app.

## Bead(s)

- `bd-efccec` — Validate libghostty build, license, and supply-chain path for Cacophony
- Related parent investigation: `bd-b0de8e` — macOS terminal embedding / libghostty direction

## Before state

- Failing tests: none known.
- Relevant metrics: the broader `bd-b0de8e` investigation existed and listed `bd-efccec` as a follow-up, but there was no dedicated supply-chain recommendation covering direct upstream libghostty vs GhosttyKit / `libghostty-spm`.
- Context: follow-up prototype beads should not add a production dependency before license, pinning, binary artifact, and CI build expectations are explicit.

## After state

- Failing tests: none known.
- Relevant metrics: new recommendation document added at `docs/investigations/bd-efccec-libghostty-supply-chain.md`; `docs/validate-pages.sh` passed with 1463 passed, 0 warnings, 0 failed.
- Context: recommendation is to use GhosttyKit only for isolated prototype speed, avoid production dependency on wrapper/prebuilt artifacts for now, and prefer a future source-built upstream libghostty/libghostty-vt path behind a Cacophony-owned adapter once prototype and broker work prove the contracts.

## Diff summary

- Commits: `bf41739f5e2a0f7d8198785a634a4f6ac81daf2f`
- Files touched: `docs/investigations/bd-efccec-libghostty-supply-chain.md`, `docs/investigations/bd-b0de8e-libghostty-embedded-terminals.md`.
- Tests: `git diff --check` and `docs/validate-pages.sh` passed.
- Behavioural delta: no runtime behavior changed; the production dependency gate is now documented before `bd-dd5935` or later macOS terminal work adds Ghostty bits.

## Operator-takeaway

GhosttyKit is acceptable for an isolated prototype, but production should not take a prebuilt/wrapper dependency yet; pin exact revisions, preserve MIT notices, prefer source-built upstream provenance, and keep all Ghostty APIs behind a local Cacophony terminal adapter.
