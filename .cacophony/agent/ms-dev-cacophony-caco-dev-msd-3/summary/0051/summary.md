# Session summary — reset stale project checkouts on config drift

## Goal

Implement bd-1ebccc so a reused project name cannot keep using a canonical daemon checkout from an older project incarnation. The work adds a secret-redacted checkout cache identity, detects drift on startup/hot reload/refresh paths, and resets stale checkouts before they can feed profile resolution, project sync, or agent shared clones.

## Bead(s)

- `bd-1ebccc` — [daemon] Reset stale project checkouts when project parameters change

## Before state

- Failing tests: not known for this bead.
- Relevant metrics: canonical checkout state only tracked sparse hash in `.cacophony/checkout-state.json`; config reload did not rebuild the checkout manager's project map or proactively initialize newly changed project checkout definitions.
- Context: Harry reported manually deleting stale `picasso` checkout directories across nodes after reinstating an old project with new parameters.

## After state

- Failing tests: none in the targeted validation run.
- Relevant metrics: `cargo test -p caco-daemon checkout::tests::` passed in queued job `tj-43ab9ad3` with 72 checkout tests; `cargo clippy -p caco-daemon --all-targets -- -D warnings` passed in queued job `tj-8d49cdad`.
- Context: canonical checkout state now persists a checkout cache fingerprint plus secret-redacted identity, config reload reconciles project checkout definitions immediately, and legacy/no-fingerprint checkouts reset once rather than being silently trusted.

## Diff summary

- Commits: `6aaa44c3c` (implementation) plus this summary commit
- Files touched: `SPEC.md`, `crates/caco-daemon/src/checkout.rs`, `crates/caco-daemon/src/config_reload.rs`
- Tests: added checkout regressions for remote change, integration-topology change, hot-reload reconcile, and legacy checkout-state reset; no tests removed.
- Behavioural delta: daemon canonical checkouts are now fingerprinted over checkout-defining project parameters, reset/recloned on mismatch, and the state file is ignored via `.git/info/exclude` so the canonical checkout remains clean.

## Operator-takeaway

A project name can now be safely reinstated with different checkout-defining parameters: the daemon detects that the cached checkout belongs to a different incarnation, logs why, and reclones instead of requiring Harry to manually delete daemon checkout directories on every node.
