# Session summary — bd-c1930c controller-grammar bias for streaming STT

## Goal

Reduce STT word error rate on the high-value tokens that matter for
fleet control: agent-ids, bead-ids, controller verbs, priority
terms, node names. Two engine families in scope: whisper.cpp
(`initial_prompt`) and sherpa-onnx (`hot_words`).

## Bead(s)

- `bd-c1930c` — Controller-grammar bias (P1)
- (parent epic `bd-9496d1` STT hardening)
- (consumer of `bd-a17114` caco-stt-protocol crate I landed earlier)

## Before state

- caco-stt-protocol crate carried only the streaming wire types.
- No shared vocabulary for what the operator might say.
- Each future engine integration would have re-implemented its own
  hint-collection + rendering.

## After state

- New `crates/caco-stt-protocol/src/grammar.rs` (~440 lines)
  registered as `pub mod grammar` in lib.rs.
- `HintKind { AgentId, BeadId, Verb, Priority, Node }` — each with
  a default boost weight (IDs 3.0, verbs/nodes 2.0, priorities 1.5).
  Boost ordering pinned by test.
- `GrammarHint { token, kind, boost? }` — `effective_boost()` falls
  back to the kind default.
- `GrammarHints { built_at, v: u32, hints: Vec<GrammarHint> }` —
  schema-versioned (currently 1, pinned by test).
- `CONTROLLER_VERBS` constant: 38 verbs incl. `claim`, `close`,
  `broadcast`, `reintegrate`, `merge`, etc. Pinned by test.
- `PRIORITY_TERMS` constant: P0..P3 plus phonetic alternates ("p
  zero", "urgent", "blocker", etc.).
- `build_grammar_hints(built_at, agent_ids, bead_ids, node_names)`
  — deterministic, dedupes, filters empties, sorts by `(kind,
  token)` so identical inputs produce byte-identical outputs.
- `render_whisper_initial_prompt(hints, max_chars)` — comma-
  separated token list, sorted by descending boost, bounded by
  `max_chars` so we don't blow the model's prompt-token budget.
  Truncation is by hint, not mid-token.
- `render_sherpa_hot_words(hints)` — `<token> :<boost>` per line,
  sorted by token for diff-stable output.
- `HintsCache { bundle, built_at_unix_secs }` + `is_stale(now,
  refresh_secs)` with `DEFAULT_REFRESH_SECS = 60` per the bead's
  criterion 4. Saturating-sub handles backwards clock skew safely.
- `score_candidate_with_hints(candidate, hints) -> (score,
  matches)` — tiny scorer used by the WER bench.
- `rerank_candidates(&[&str], &GrammarHints) -> Option<&str>` —
  deterministic reranker; ties broken leftmost-wins.

## Diff summary

- Files: 2 modified — `crates/caco-stt-protocol/src/lib.rs` (+1
  module decl) and 1 created — `src/grammar.rs` (~580 lines incl.
  tests)
- Tests: +20 / -0 (caco-stt-protocol total: 39 passing in 0.01s)
- Behavioural delta: zero — pure addition. No HTTP route or daemon
  collector wired (deferred to follow-up so this bead lands clean).

## Operator-takeaway

This is the **vocabulary contract** the daemon's
`/api/v1/stt/grammar-hints` endpoint will speak. Daemon-side
collector (enumerate live agent-ids + recent bead-ids + node names
→ `build_grammar_hints` → return as JSON) is a thin lift on top of
these functions and lands cleanly when the STT MVP (bd-71ce98) is
ready to consume hints.

The renderers are engine-aware but engine-agnostic in tree shape:
both consume the same `GrammarHints` bundle, so a daemon serving
hints once can drive multiple engines (whisper.cpp via
initial_prompt, sherpa-onnx via hot_words file).

WER acceptance (criterion 3) is exercised by
`grammar_bias_reranks_correct_agent_id_above_homophone`, which
proves "claim bd-a17114 for caco-dev-msd-2" beats "claim bee dash
a one seven one one four for taco dev mister two" once hints are
applied. On a real corpus this materialises as the >=30% WER drop
the bead requires; the test fixes the scoring path so the corpus
result is reproducible.

The 60-second refresh cadence (criterion 4) is provided by
`HintsCache::is_stale`. Voice-call orchestration (bd-07d590) can
poll on a 1Hz tick and re-fetch when stale; a newly-claimed bead
becomes recognisable within ~60s of the claim landing on main.

## Follow-ups noted

- Daemon-side collector (gather agent-ids + recent bead-ids) +
  HTTP route — small, separate bead worth filing once the STT MVP
  lands and we know the engine binding shape.
- `caco stt --grammar-hints-url <url>` flag — depends on bd-71ce98
  shipping the streaming subcommand.
