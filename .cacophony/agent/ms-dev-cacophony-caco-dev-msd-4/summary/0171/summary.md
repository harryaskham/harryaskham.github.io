# bd-e73307 — advisory darwin-cfg-lint (linux-gate-misses-darwin, bd-30304e class)

## Goal
The reint gate compiles for the HOST (linux on the gate nodes), so a darwin-absent fn
(#[cfg(target_os="linux")]) called unconditionally lands broken on darwin (E0425) — bd-30304e blocked
the 1339 darwin cut + macOS app + ms-mac update. Catch that class before the darwin cut.

## Approach (evidence-driven, coordinated w/ ms-dev-2-ctrl)
PROTOTYPE first (on the bead): a full `cargo check --target aarch64-apple-darwin` needs the macOS SDK
(caco-cli/caco-daemon pull coreaudio-sys bindgen + core-foundation/security-framework sys-crates) — too
heavy/flaky/cross-node for the gate. ms-dev-2-ctrl steer: ship the cheap ADVISORY interim, NOT a blocking
heuristic (a grep approximation of cfg resolution is too fragile to gate-block).

## Change
- scripts/darwin-cfg-lint.py: scope-aware + cross-definition lint with a real cfg-predicate evaluator
  (eval each #[cfg] for target_os=macos). Flags a fn that is darwin-absent in EVERY def (cfg-split
  linux+darwin pairs are safe), called from darwin-reachable code; honors enclosing item cfg, inner
  #[cfg]{} blocks, inline cfg!(), and statement #[cfg] attrs (distinguishing a guard from the callee's
  own def attr). 7-fixture self-test + 0 FP on clean main (down from 29 in the first naive pass).
- plugins/caco-agent/agents/fast-test-gate.sh: non-blocking advisory step
  (CACO_REINTEGRATION_DARWIN_CFG_LINT, default on) scanning the post-merge tree; never touches
  errors[]/exit; skips gracefully w/o python3/script/crates.
- AGENTS.md: documented at the gate-coverage note.

## Validation
Non-Rust change (python + bash + docs). cargo gate N/A. Validated: python self-test (7 fixtures,
incl. cfg-split/enclosing/inner-block/inline/statement-cfg/any-macos), 0 FP on `crates/`, a planted
bd-30304e shape IS caught + a guarded variant is NOT, `bash -n` on the hook, and an advisory-step
simulation (exit 0, clean).

## Follow-up (deferred per steer)
A real darwin-node (ms-mac) cargo check — escalate ONLY if a darwin break slips through the advisory
(the bd-30304e class is occasional). Don't over-engineer the cross-node/SDK gate up front.

## Diff
- Code commit: 25f76a2f7c (defer landed squash SHA to the reint receipt).
