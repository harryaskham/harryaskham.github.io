# Session summary — bd-e13efd caco-cli extraction wave

## Goal

Shrink the giant `crates/caco-cli/src/lib.rs` monolith by moving coherent command families into sibling modules without breaking the source-grep-pinned dispatcher tests that still require thin shells in `lib.rs`. The goal for this session chunk was not to finish every possible wrapper cut, but to reach an honest, rebased checkpoint where the largest remaining families were extracted, the post-rebase API drifts were reconciled, and the branch was validated strongly enough to land.

## Bead(s)

- `bd-e13efd` — Project health: `caco-cli/src/lib.rs` is huge; split into submodules to improve iteration speed

## Before state

- `origin/main` at ship time still kept large CLI family implementations inline in `crates/caco-cli/src/lib.rs`.
- On the rebased baseline, `crates/caco-cli/src/lib.rs` measured `102554` lines.
- Command families now extracted in this session were still tangled into the main CLI monolith before this wave: msg/audio, tmux, service, sidecar, scratch, release, outbox, bootstrap-dev, profile list/show helpers, event log, node renderers/status assembly, and config-show.
- Ship-prep surfaced two stale `caco-cli` tests (`choices_subcommands_are_registered`, `config_schema_section_filter_csv_keeps_multiple_sections_bd2caf68`) plus one post-rebase signature drift in extracted `msg history`; these had to be reconciled before the extraction wave could land cleanly.

## After state

- `crates/caco-cli/src/lib.rs` now measures `91373` lines on top of current main.
- The extracted family/helper modules now present on the rebased branch are:
  - `crates/caco-cli/src/msg_cmd.rs`
  - `crates/caco-cli/src/audio_cmd.rs`
  - `crates/caco-cli/src/service_cmd.rs`
  - `crates/caco-cli/src/sidecar_cmd.rs`
  - `crates/caco-cli/src/scratch_cmd.rs`
  - `crates/caco-cli/src/release_cmd.rs`
  - `crates/caco-cli/src/outbox_cmd.rs`
  - `crates/caco-cli/src/bootstrap_cmd.rs`
  - `crates/caco-cli/src/profile_cmd.rs`
  - `crates/caco-cli/src/event_cmd.rs`
  - `crates/caco-cli/src/node_cmd.rs`
  - `crates/caco-cli/src/config_show_cmd.rs`
- `lib.rs` retains the thin source-pinned shells where tests still require them, but the heavy implementations moved out into the sibling modules above.
- Post-rebase validation is green for the touched crate: `cargo build -p caco-cli`, `cargo clippy -p caco-cli --all-targets --no-deps -- -D warnings`, and the targeted regression matrix listed below all pass.

## Diff summary

- Commits: `a598c4ed9` … `0fd5b677e` (20 `bd-e13efd` commits rebased onto current `origin/main`)
- Files touched: `crates/caco-cli/src/lib.rs`, `crates/caco-cli/src/msg_cmd.rs`, `crates/caco-cli/src/audio_cmd.rs`, `crates/caco-cli/src/service_cmd.rs`, `crates/caco-cli/src/sidecar_cmd.rs`, `crates/caco-cli/src/scratch_cmd.rs`, `crates/caco-cli/src/release_cmd.rs`, `crates/caco-cli/src/outbox_cmd.rs`, `crates/caco-cli/src/bootstrap_cmd.rs`, `crates/caco-cli/src/profile_cmd.rs`, `crates/caco-cli/src/event_cmd.rs`, `crates/caco-cli/src/node_cmd.rs`, `crates/caco-cli/src/config_show_cmd.rs`
- Net diff vs current `origin/main`: 14 files changed, `35943` insertions, `35702` deletions; in the main monolith itself, `lib.rs` dropped from `102554` lines on `origin/main` to `91373` lines on this rebased branch.
- Behavioural delta:
  - Moved full or heavy partial implementations for msg/audio, bootstrap-dev, outbox, release, scratch, sidecar, service, event-log, config-show, and node-family rendering/assembly out of `lib.rs`.
  - Preserved source-grep-sensitive wrapper shells in `lib.rs` where tests still require exact dispatcher names/strings.
  - Reconciled the extracted `msg history` helper with current-main `--metadata-only` / `--body-preview` support after rebase.
  - Kept current-main stale-test fixes during rebase for `choices` subcommand registration and config-schema JSON envelope assertions.
- Validation run after rebase:
  - `cargo build -p caco-cli`
  - `cargo clippy -p caco-cli --all-targets --no-deps -- -D warnings`
  - `cargo test -p caco-cli tests::choices_subcommands_are_registered -- --exact --nocapture`
  - `cargo test -p caco-cli tests::config_schema_section_filter_csv_keeps_multiple_sections_bd2caf68 -- --exact --nocapture`
  - `cargo test -p caco-cli tests::node_status_unknown_node_error_matches_node_show_wording -- --exact --nocapture`
  - `cargo test -p caco-cli tests::dispatch_msg_snapshot_rejects_empty_agent -- --exact --nocapture`
  - `cargo test -p caco-cli tests::audio_speak_request_body_includes_express_as_when_set -- --exact --nocapture`
  - `cargo test -p caco-cli tests::build_test_release_list_validate_project_first -- --exact --nocapture`
  - `cargo test -p caco-cli tests::release_list_validates_channel -- --exact --nocapture`
  - `cargo test -p caco-cli tests::profile_list_validates_source_filter -- --exact --nocapture`
  - `cargo test -p caco-cli tests::dispatch_profile_show_emits_structured_error_envelope_on_json_not_found -- --exact --nocapture`
  - `cargo test -p caco-cli tests::event_log_args_include_project_and_type_alias -- --exact --nocapture`
  - `cargo test -p caco-cli tests::config_show_section_filter_yaml_keeps_only_named_section_bd2b096e -- --exact --nocapture`
  - `cargo test -p caco-cli tests::config_show_path_invalid_expression_errors_with_bead_id_bd_deec53 -- --exact --nocapture`
  - `cargo test -p caco-cli tests::bootstrap_dev_unified_dispatcher_exposed -- --exact --nocapture`

## Operator-takeaway

This is a real extraction checkpoint, not a cosmetic shuffle: the big `caco-cli` monolith is now materially smaller on top of current main, the heaviest remaining families live in dedicated sibling modules, and the rebase proved the extraction can survive concurrent mainline movement without discarding current behaviour. The remaining work on `bd-e13efd` is now mostly thinner wrapper surgery and test-contract cleanup rather than more obvious family-scale seams.