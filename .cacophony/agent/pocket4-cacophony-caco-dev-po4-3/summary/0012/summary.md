# Session summary — first-party temp checkout cleanup command

## Goal

Respond to renewed ms-mac disk pressure after the initial prevention fix landed by adding a first-party, operator-approvable cleanup command for existing stale direct-integration temp checkouts, rather than deleting remote temp files manually from the worker session.

## Bead(s)

- `bd-07e3f9` — [regression-after-close] ms-mac disk free drops below 50Gi again

## Before state

- Failing tests: none for this slice.
- Relevant metrics: log-monitor reported ms-mac dropped again to about 28.05 GiB free / 97% used. Read-only SSH confirmed `/private/var/folders/.../T` remained about 169 GiB with many stale `nix-shell*/caco-direct-integration-*` directories, commonly around 7 GiB each.
- Context: existing `caco prune run` cleanup modes covered agent dirs, cargo targets, Pi package cache/session history, and TUI logs, but not these direct-integration tempdirs. Doctor correctly refused manual deletion without explicit operator approval or a first-party surface.

## After state

- Failing tests: none observed for focused validation.
- Relevant metrics: no remote cleanup was performed by this worker. A new command is available for deploy/use: `caco prune run --direct-integration-tempdirs --dry-run --max-age-hours 4 --json` for preview and `caco prune run --direct-integration-tempdirs --delete --max-age-hours 4` for the destructive, prefix-scoped cleanup.
- Context: the command searches local `nix-shell*` temp roots, including visible macOS `/private/var/folders/*/*/T/nix-shell*/...` roots, and targets only directory basenames starting with `caco-direct-integration-` older than the configured age.

## Diff summary

- Code/content commits: `0109fbe5a`.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched: `crates/caco-cli/src/lib.rs`, `SPEC.md`, `README.md`.
- Tests: focused `caco-cli` prune help tests passed; `cargo run -p caco -- prune run --help --json` exposed `--direct-integration-tempdirs` and `--max-age-hours`; local dry-run JSON command executed successfully.
- Behavioural delta: Cacophony now has a bounded first-party cleanup path for abandoned direct-integration temp checkouts, with dry-run preview and explicit `--delete` for destructive cleanup.

## Operator-takeaway

The live ms-mac space recovery still requires running the new first-party cleanup after deploy/approval; this session added the command path so doctor/operator remediation no longer has to choose between manual deletion and waiting.
