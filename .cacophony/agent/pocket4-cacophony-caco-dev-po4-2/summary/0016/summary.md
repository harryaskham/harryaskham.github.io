# Session summary — persistent PR reintegration now warns loudly about live runtimes

## Goal

Investigate why `msm-5` appeared not to be recreated after PR-backed reintegration and make that outcome explicit and durable so operators do not mistake the preserved runtime for a failed recreate path. The aim was not to force destructive rotation during PR review, but to surface the intended workflow clearly and loudly enough that future PR reintegrations cannot strand operators in ambiguity.

## Bead(s)

- `bd-3a5a0f` — `[pr-integration] msm-5 reintegration did not recreate the agent as expected`
- `bd-167bd6` — draft follow-up: generate shipped profile docs tables from `.cacophony/profiles` instead of hand-maintained HTML rows

## Before state

- Operator report: `msm-5` reintegration did not recreate the agent as expected.
- The relevant worker fleet (`caco-dev-msm-*`) is defined under `.cacophony/agents/cacophony_persistent.yaml`, so these are persistent dev agents rather than disposable one-shot workers.
- `caco agent reintegrate` already had a CLI-only warning helper for persistent PR-backed reintegration, and SPEC already said PR-backed reintegration for persistent agents must not silently imply recreation.
- The remaining gap was observability and workflow clarity:
  - the warning was easy to miss if the CLI output was not the main operator surface
  - repo/user docs did not state the expected post-merge `caco agent recreate <agent-id>` follow-up clearly enough

## After state

- `caco agent reintegrate` now emits a feed/audit warning event when a persistent agent successfully uses `pr_review` or `pr_auto_merge` and therefore remains live.
- The warning is still present in CLI output / JSON, but now also persists via the daemon feed path with source `reintegration.persistent_pr_live_runtime`.
- README / AGENTS / docs/agents now state the intended workflow explicitly:
  - PR-backed mid-flight reintegration on persistent agents is intentionally non-destructive
  - the runtime stays alive until the PR lands
  - operators/controllers should run `caco agent recreate <agent-id>` afterward when they want a fresh runtime
- Root cause conclusion:
  - this was primarily a workflow/observability gap, not a daemon failure to recreate a persistent agent at the wrong moment

## Diff summary

- Files touched:
  - `crates/caco-cli/src/lib.rs`
  - `README.md`
  - `AGENTS.md`
  - `docs/agents.html`
- Behavioural delta:
  - successful persistent PR reintegration now emits a durable warning event instead of relying only on transient CLI text
  - operator docs now explain that PR-backed reintegration preserves the runtime and requires explicit recreate after merge if rotation is desired
- Validation:
  - `cargo build -p caco-cli`
  - attempted `cargo test -p caco-cli persistent_pr_warning -- --nocapture`, but main is currently broken for unrelated reasons in `caco-cli` lib tests (`dispatch_codespace_new_pushes_rendezvous_bootstrap_secret_bd_0bed93` duplicate definition), already claimed by helsinki
  - attempted `cargo test-small`, but hit unrelated known broken-on-main docs/profile drift (`shipped_profiles_html_lists_every_canonical_profile` / missing `caco-macos` row), already claimed by `po4-1`
- Regression coverage added in-tree (currently blocked from execution by the unrelated duplicate-test breakage above):
  - `persistent_pr_warning_event_is_written_to_feed`
  - `persistent_pr_warning_event_skips_direct_mode`

## Operator-takeaway

`msm-5` did not expose a hidden recreate bug so much as an under-signalled persistent-agent workflow: PR-backed reintegration is supposed to preserve context until merge. The fix here makes that outcome visible in durable operator surfaces and documents the follow-up recreate step so future PR-based reintegrations do not look like silent failures.
