# Session summary — label graphics delete churn separately

## Goal

Continue the caco-tui optimizer cycle after closing `bd-45c7a3`, then address reopened `bd-58dcaa` evidence that showed intermittent TUI graphics upload bursts after prior cache-label fixes. The goal was to separate high delete churn from cache invalidation in telemetry so the remaining intermittent burst path is diagnosable instead of repeatedly reopened as generic `cache:zero_hit`.

## Bead(s)

- `bd-58dcaa` — Investigate TUI graphics zero-cache upload bursts on idle ms-mac

## Before state

- Failing tests: none known at start.
- Relevant metrics: latest reopened evidence included a one-pass cold burst and a nearby 3-graphics-pass sample with 194 uploads, 225 deletes, 12% cache hit, ~10 MiB wire, and ~76ms average upload pass. The latter had cache hits, so it was not pure zero-hit cache invalidation, but existing labels did not call out delete churn.
- Context: `bd-cdb4dd` already split short warmup windows from sustained `cache:zero_hit`; this slice handles the other visible reopened shape: active surface retirement/delete churn paired with upload bursts.

## After state

- Failing tests: none observed.
- Relevant metrics: `PerfFlush::labels()` now emits `delete:churn` when a window has at least 100 deletes and deletes are greater than or equal to uploads. The new focused test models the reopened 225-delete / 194-upload sample and verifies it is labelled `delete:churn`, `upload:burst`, and `upload_wire:large`, while not being labelled `cache:zero_hit` because cache hits exist.
- Context: this does not reduce upload bytes directly; it determines that the non-warmup reopened sample is a delete-churn/active-retirement shape, not plain cache invalidation. Future perf-list evidence should now distinguish cold warmup, sustained zero-hit, and delete churn.

## Diff summary

- Commits: final branch/reintegration commit to be assigned by `caco agent reintegrate`.
- Files touched: `crates/caco-tui/src/perf.rs`, `SPEC.md`, `docs/tui.html`.
- Tests: added `flush_labels_delete_churn_bd_58dcaa`; no tests removed.
- Behavioural delta: no TUI rendering/upload scheduling semantics changed; graphics perf labels now include `delete:churn` for high-delete upload windows.
- Validation: `./scripts/rustfmt-changed.sh`; `docs/validate-pages.sh`; `git diff --check`; queued `cargo test -p caco-tui flush_labels_delete_churn_bd_58dcaa` (`tj-d853d20b`); queued `cargo check -p caco-tui` (`tj-ab6194d7`); queued `cargo clippy -p caco-tui --lib -- -D warnings` (`tj-90d4c25a`); queued `cargo test -p caco-tui` (`tj-caaf1d91`).

## Operator-takeaway

The reopened graphics-burst evidence now has a separate label for the high-delete path. If future samples still show `cache:zero_hit` above the warmup threshold without `delete:churn`, that is stronger evidence of real cache invalidation; if they show `delete:churn`, the next optimisation should target surface retirement churn instead.
