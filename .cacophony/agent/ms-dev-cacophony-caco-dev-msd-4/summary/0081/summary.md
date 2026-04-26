# Session summary — standalone TTS fixture guidance

## Goal

Resolve the ms-mac standalone `tts` CLI Python/Numpy ABI failure by removing it from the recommended managed-agent fixture path, rather than trying to repair an unowned ambient user-profile package from this repository.

## Bead(s)

- `bd-18cd2f` — Standalone tts CLI fails on ms-mac with Python/Numpy ABI mismatch

## Before state

- Failing tests: not applicable; this was a documentation/config guidance bug around an external host-provided CLI.
- Relevant metrics: `docs/transcription.md` still described the standalone `tts` CLI as a suggested/potential non-daemon synthetic speech generator while also noting that it failed on the configured macOS runner.
- Context: the bead acceptance allowed either making `tts --help` work in the managed ms-mac environment or stopping the docs/config from recommending the standalone CLI where it is not packaged coherently.

## After state

- Failing tests: none in documentation validation.
- Relevant metrics: the transcription guide, published HTML, README, AGENTS guidance, and SPEC now prefer `caco audio speak --output` for deterministic speech fixtures and warn against unowned ambient `tts` binaries unless declared in config and verified on the host.
- Context: no active TTS probe was run; the fix uses the documented fallback path already known to work through the daemon/provider.

## Diff summary

- Commits: `5414d33e`
- Files touched: `docs/transcription.md`, `docs/transcription.html`, `README.md`, `AGENTS.md`, `SPEC.md`
- Tests: `docs/validate-pages.sh`
- Behavioural delta: managed agents should no longer choose the broken standalone `tts` binary on ms-mac for synthetic fixtures; they should use the first-party daemon-backed audio path unless a local generator is explicitly configured and verified.

## Operator-takeaway

The ABI-mismatched standalone `tts` binary is no longer part of the recommended Cacophony transcription workflow; this closes the failure by removing unsafe guidance while preserving the daemon-backed fixture path.
