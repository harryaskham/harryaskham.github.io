# Session summary — fix stack-overflow in startup profile discovery

## Goal

Fix bd-e03433: a `caco-daemon` library test
(`discover_available_profile_names_includes_checked_in_canonical_profiles_without_checkouts`)
crashes with `fatal runtime error: stack overflow, aborting` when run
against the real cacophony repo's `.cacophony/` overlay. The function
under test is on the daemon startup path, so any worker reproducing
this would also be at risk on small-stack call sites.

## Bead(s)

- `bd-e03433` — [broken-on-main] discover_available_profile_names_includes_checked_in_canonical_profiles_without_checkouts stack overflow

Side-effect (not blocking this bead): broadcasted and queued an
umbrella bead for 8 unrelated `caco-daemon` library failures all
returning `422 Unprocessable Entity` from a recent caller-header
validation change (bd-43db78). Filing was queued for retry because
beads-primary was unreachable at the time; verifying my own fix did
not depend on this.

## Before state

- Failing tests:
  - `discover_available_profile_names_includes_checked_in_canonical_profiles_without_checkouts`
    crashes with stack overflow when run as a normal `cargo test`
    worker thread (~2 MB stack). Passes on the main thread (8 MB).
- Repro: `cargo test -p caco-daemon --lib discover_available_profile_names_includes_checked_in_canonical_profiles_without_checkouts`
- Root cause: `caco_config::overlay::discover_checkout_profile_names`
  unconditionally called `load_config_from_path` on the project's
  `.cacophony/config.yaml`, which renders the full template/import
  graph (`projects.yaml`, `automation.yaml`, `tui.yaml`,
  `generation.yaml`, and their nested imports). The recursive render
  blows the worker stack on real cacophony-repo overlay sizes.

## After state

- Failing tests: none for bd-e03433. Original test passes:
  `test tests::discover_available_profile_names_includes_checked_in_canonical_profiles_without_checkouts ... ok`
- New regression coverage in `crates/caco-config/src/overlay.rs`:
  - `discover_checkout_profile_names_no_profiles_key_is_cheap_on_small_stack`
    runs the function on a 256 KB stack and asserts no overflow.
  - `raw_text_has_top_level_profiles_key_matches_only_top_level` covers
    the cheap textual-scan corner cases (top-level vs nested,
    `profiles_disabled:` false-positive guard, comments, empty input).
- `cargo test-small`: 239 passed, 0 failed.
- `cargo test -p caco-config --lib`: 772 passed, 0 failed.
- `cargo clippy -p caco-config`: clean.

## Diff summary

- Commit: `cf0889531` ("bd-e03433: short-circuit
  discover_checkout_profile_names when no top-level profiles: key")
- Files touched: `crates/caco-config/src/overlay.rs` (1 file, +95 / -18)
- Tests: +2 (one stack-overflow regression, one textual-scan unit
  test); 0 removed; 0 flipped.
- Behavioural delta: `discover_checkout_profile_names` now performs a
  cheap top-level `profiles:` literal check on the raw overlay text
  before invoking the template renderer. When that key is absent (the
  common case — the cacophony repo declares all profiles as `*.md`
  files, not inline overlay entries) the function returns immediately
  without touching the renderer. When present, behaviour is identical
  to before (validate keys → render → parse overlay → extract
  profile names).

## Embedded artefacts

(none)

## Operator-takeaway

The startup profile-discovery path was paying the full config-template
render cost just to ask "are there any inline profile declarations
here?" — and on production-sized overlays that recursive render
overflows the small stack used by `cargo test` worker threads. The
fix is a one-line textual short-circuit guarded by two regression
tests, but the broader lesson is: **hot startup paths must avoid
invoking the full config-template renderer unless they actually need
the rendered config**. There may be other discovery/inspection
callsites that fall into the same trap; worth a separate audit pass
when someone has cycles.
