# Session summary — controller managed-constraint cleanup

## Goal

Remove role-conflicting managed runtime safety wording from controller reified instructions so generic no-plan/autonomy snippets no longer tell persistent controllers to behave like implementation workers.

## Bead(s)

- `bd-8bf415` — Profile audit: controller reified instructions include worker lifecycle conflicts

## Before state

- Failing tests: none owned for this bead at start; unrelated macOS smoke broken-on-main reports were acknowledged as owned elsewhere.
- Relevant metrics: checkout-injected Claude and Codex managed constraint snippets used implementation-oriented wording such as “proceed directly with implementation,” while controller profiles are persistent, may have no assigned bead, and must not expose `caco agent complete` as a generic success path.
- Context: role-aware profile instructions already excluded worker lifecycle blocks for controller scopes, but the extra managed runtime snippets outside the canonical profile body still needed to be role-neutral.

## After state

- Failing tests: none from focused validation.
- Relevant metrics: queued job `tj-035f020b` passed `CARGO_INCREMENTAL=0 CARGO_PROFILE_TEST_DEBUG=0 cargo test -p caco-daemon --lib managed_checkout_constraints_are_role_neutral_for_controller_profiles_bd_8bf415 -- --nocapture`.
- Context: Claude/Codex managed checkout snippets now defer to the resolved profile/current task, avoid implementation-specific wording, and explicitly note that persistent/controller agents may have no assigned bead and must use their profile lifecycle.

## Diff summary

- Code/content commits: `04942fa486`
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `crates/caco-daemon/src/agent/spawn.rs`, `crates/caco-daemon/src/agent/tests.rs`, `SPEC.md`
- Tests: +1 / -0 / flipped 0; focused queued caco-daemon lib test passed in `tj-035f020b`
- Behavioural delta: managed runtime safety snippets remain autonomous/no-plan guardrails but no longer imply that every managed agent is a feature-implementing bead worker.

## Operator-takeaway

Controller/profile-specific lifecycle instructions now have a cleaner precedence boundary: generic injected safety text forbids plan/confirmation behavior, while assigned-bead and completion semantics are left to the resolved profile instead of leaking worker assumptions into controllers.
