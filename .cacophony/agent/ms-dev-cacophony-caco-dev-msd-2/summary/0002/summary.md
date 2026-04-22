# Session summary — bd-f49a71 actually wire sccache for worker compiles

## Goal

Stop the compile fanout that saturates helsinki: ~24 worker checkouts
each running cold cargo builds in parallel because the shared sccache
that was nominally configured was never actually being invoked.

## Bead(s)

- `bd-f49a71` — Compile fanout saturates helsinki: concurrent workers
  each run independent cargo builds with no shared cache (P1 feature)

## Before state

- `sccache --show-stats` reported 0 compile requests despite sccache
  being in the dev shell and `.envrc` claiming to set RUSTC_WRAPPER.
- Two bugs combined to silently disable sccache:
  1. `.envrc` resolved `command -v sccache` BEFORE `use flake`, so
     sccache wasn't on PATH yet and `RUSTC_WRAPPER` was set to the
     empty string. cargo treats `RUSTC_WRAPPER=` as no-wrapper.
  2. `.envrc` requires direnv. Worker agent shells / manual `nix
     develop` / CI invocations bypass it entirely.
- Result: every worker did a cold rustc compile of caco-daemon,
  ~24 in parallel during dispatch storms.
- Failing tests: none.

## After state

- `.envrc` reordered: `use flake` activates first, then `command -v
  sccache` resolves to the real binary, then `RUSTC_WRAPPER` is set.
- Same wrapper export added to the flake's default devShell
  `shellHook` so every consumer of `nix develop` (direnv, manual
  shells, CI, scripts) gets sccache wired automatically.
- Verified live in the agent checkout:
  `sccache --show-stats` went from 0 compile requests before the
  fix to 382 after one `cargo test-small` invocation.
- `cargo test-small` passes (45 green) under the new wrapper.
- Failing tests: none.

## Diff summary

- Commit: `ac52cac6`
- Files touched:
  - `.envrc` — reorder so `use flake` precedes RUSTC_WRAPPER export
  - `flake.nix` — add `shellHook` exporting `RUSTC_WRAPPER` and
    `SCCACHE_DIR` for the default devShell
- Tests: 0 added (this is a build-environment fix; the regression
  signal is `sccache --show-stats` reporting non-zero requests after
  any cargo invocation, which is operationally observable).
- Behavioural delta: every cargo invocation from the dev shell now
  routes through sccache; the shared `$HOME/.cache/sccache` directory
  collapses N parallel cold compiles of caco-daemon across worker
  checkouts into one cold compile plus N-1 cache hits.

## Operator-takeaway

This is the lowest-risk attack on the helsinki saturation pattern from
tonight's outage. The `.envrc` ordering bug had been silently
degrading every worker compile for an unknown amount of time; the
flake `shellHook` ensures the regression cannot repeat for any consumer
of the dev shell. The other bd-f49a71 candidates (per-node concurrency
caps on cargo-invoking workers, target-dir hardlink inheritance,
scheduler caps) remain valid follow-ups but are higher-risk and should
be measured against the real sccache hit-rate before being prioritised.
Operators should monitor `sccache --show-stats` on helsinki over the
next dispatch storm to confirm the cache hit rate climbs into the
high-hit regime expected for incremental work on a shared base.
