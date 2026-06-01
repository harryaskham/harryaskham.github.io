# bd-6a2716 — quickfile AI bead generation: gate + filter against speculative non-SPEC beads

## Problem
The quick-file AI bead-expansion path (`POST /api/v1/projects/<project>/beads/expand`,
`handle_expand_beads`) called the LLM and persisted every returned proposal without any
context gating or output filtering. On 2026-06-01 ~02:51, a low-context quick-file use
generated a burst of 5 speculative P0 beads (bd-5ac55b/fc21f2/ba57a0/da50fb/6d9965) under
the operator's identity — generic Prometheus/OpenTelemetry observability boilerplate that
referenced Go goroutines (in a Rust project) and proposed non-SPEC architecture. Operator
(Harry) diagnosed the root cause: quickfile AI generation didn't get enough context.

Root cause (per caco-dev-msm-3 investigation, confirmed): well-tested PURE gate/filter
helpers already existed in `crates/caco-daemon/src/suggest_beads_context.rs`
(`evaluate_suggest_beads_context_gate`, `filter_vague_suggest_beads_output`) but had ZERO
callers outside their own module — they were never wired into the live expand path.

## Change
`crates/caco-daemon/src/beads.rs` `handle_expand_beads`:
1. BEFORE LLM generation: build a `SuggestBeadsSourceContext` from `body.text` + `body.context`
   + `project`, run `evaluate_suggest_beads_context_gate()`. Empty/too-thin(<96B)/too-broad
   (>64KiB)/missing-project contexts return `422 expand_beads_insufficient_context` with the
   gate code, byte size, bounds, and guidance — instead of hallucinating boilerplate.
2. AFTER generation, BEFORE persistence: convert proposals → `RawSuggestBeadsCandidate`, run
   `filter_vague_suggest_beads_output()`, keep only surviving (deduped-by-title) proposals.
   Rejections surface in the response (`rejected` / `rejected_count`), in the dry_run response,
   and via an eprintln diagnostic. If everything is filtered, return
   `422 expand_beads_all_proposals_filtered` and file nothing.

`crates/caco-daemon/src/suggest_beads_context.rs`:
- Added `SUGGEST_BEADS_FOREIGN_STACK_NEEDLES = ["goroutine"]` and a distinct `foreign_stack`
  reject code in the filter. Intentionally narrow / near-zero false positive: only "goroutine"
  (a Go-runtime term with no valid use in this Rust project). Monitoring-stack terms
  (prometheus/opentelemetry/grafana) are deliberately NOT rejected so a legitimate
  "do NOT add a Prometheus endpoint" bead survives.

`README.md`: updated the Suggest Beads passage to state the helpers are now wired into the live
expand endpoint with the 422 gate + foreign-stack filter behavior (bd-6a2716).

## Tests
Added in suggest_beads_context.rs:
- `filter_drops_foreign_stack_goroutine_bd_6a2716` — Go "goroutine" bead rejected (foreign_stack),
  real Rust bead kept.
- `filter_keeps_legitimate_prometheus_mention_bd_6a2716` — "do not add Prometheus" bead survives.
- `gate_refuses_thin_and_allows_bounded_bd_6a2716` — empty/thin refused, bounded draft allowed.

## Validation (queued, shared host)
- `cargo check -p caco-daemon --tests` — ok (exit 0)
- `cargo test -p caco-daemon --lib suggest_beads` — 7 passed (3 new + 4 existing)
- `cargo test -p caco-daemon --lib expand_beads` — 2 passed (no regression)
- `cargo clippy -p caco-daemon --tests` — ok (exit 0), no warnings on changed code

## SPEC areas
Beads CRUD / quick-file AI assistance; SPEC bans hidden non-SPEC architecture. This makes the
live expand path refuse low-context speculative generation rather than filing it. No new
hidden architecture; reuses existing pure helpers.

## Scope note
Deliberately did NOT touch the caco-tui quickfile UI surface (`app.rs` expand_beads callers) —
the daemon-side gate/filter is the authoritative enforcement point and protects all callers
(TUI, web, API) uniformly.
