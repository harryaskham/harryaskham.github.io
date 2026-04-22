# bd-bf1e86 polish #9: truncate_label ellipsis for caco bd graph text/dot/mermaid labels

## Goal

Polish track #9: replace 5 `chars().take(N).collect()` silent-chop sites in `crates/caco-cli/src/lib.rs::render_dot/render_mermaid/render_ascii` (operator-facing graph output for `caco bd graph`) with a shared `truncate_label(s, max)` helper that appends `"..."` when the string actually exceeds `max`. Mirrors `caco-tui::views::common::truncate` (used in inbox preview, polish #6) and the same antipattern fixed in TUI cycles.

## Bead(s)

- bd-bf1e86 (permanent polish bead — polish #9 of N this session)

## Before state

`render_dot`, `render_mermaid`, and `render_ascii` (3 call sites in `render_ascii` — main title, parent title, dep title) all used the same idiom:
```rust
let title_short: String = title_esc.chars().take(60).collect();
```
which silently truncates mid-word. For `caco bd graph --format text` (used in beads-tree printout) this means a long bead title looks identical-but-shorter to a genuinely-short one.

No tests for the chop behaviour at all.

## After state

- New `truncate_label(s: &str, max: usize) -> String` helper at module scope (just below `render_mermaid`), with doc-comment cross-referencing the TUI helper. Same shape: char-based count, ellipsis when `max > 3`, hard chop when `max <= 3`.
- All 5 sites in `render_dot`/`render_mermaid`/`render_ascii` updated:
  - `render_dot`: title chop 60
  - `render_mermaid`: title chop 50
  - `render_ascii`: title chop 60, parent_title chop 40, dep_title chop 40
- 5 new unit tests in `mod tests`:
  - `truncate_label_short_string_unchanged`
  - `truncate_label_at_boundary_unchanged`
  - `truncate_label_long_string_gets_ellipsis`
  - `truncate_label_handles_unicode_boundaries` (multi-byte chars)
  - `truncate_label_tiny_max_no_ellipsis`

Verification:
- `cargo test -p caco-cli --lib truncate_label`: 5/5 PASS
- `cargo test-small`: 57/57 PASS
- `cargo clippy --workspace --all-targets -- -D warnings`: clean

## Side note: cluster bd-subsystem wedge during this cycle

Mid-cycle, `test-user-hel` and peers reported every `caco bd` command failing with `CHECK constraint failed: length(title) >= 1 AND length(title) <= 500` — a single malformed peer mutation wedged the whole bd-sync loop fleet-wide. I drafted a defensive clamp in `index_mutation_in_tx` (and 2 unit tests pinning it) but discovered peer wmi-1 had landed an identical fix during the same cycle (commit f4708ae6+). Discarded my duplicate, kept my polish #9 work. Cluster recovered automatically when daemons restarted.

This is a useful pattern: when an outage report comes in, the right move is to draft a fix locally first (so you have a working diagnosis if peer fixes don't materialize), then defer to peer's landed fix when discovered. Net cost: ~10min of local diagnosis time, no merge-conflict pain since I git-checkout'd the file before pulling.

## Diff summary

1 file changed, +51 / −10:

- `crates/caco-cli/src/lib.rs`:
  - +18 / −0 helper `truncate_label`
  - +5 / −5 call-site replacements (5 `chars().take().collect()` → `truncate_label`)
  - +28 / −0 unit tests

## Operator-takeaway

**Polish #9** completes the silent-chop-with-ellipsis pattern fix in caco-cli graph rendering. Operators using `caco bd graph --format text` now see `[bd-12345 open] Some bead title that gets long en...` instead of the old `... gets long e` — small win for diff-readability when piping graph output into PRs/notes.

Tests pin the contract so future graph-format additions can use the helper safely.

**Cluster bd-subsystem wedge** earlier this cycle was resolved by peer wmi-1's defensive clamp landing on main. The class of bug — a single malformed peer mutation wedging the sync loop — is now fixed, with a regression test pinning it. Worth filing follow-ups for: (a) write-time validation in `bd update`/`bd create` (mine + peer's fixes are read-side; the upstream client that produced the bad title can still create new ones), (b) per-mutation error-isolation generally (one bad row shouldn't be able to take down a whole reimport, even outside the title-length case).
