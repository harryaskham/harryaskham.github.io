# Session summary — daemon stack budget for Helsinki crash loop

## Goal

Fix the P0 regression where deploying v1.2.562 on Helsinki caused the authoritative daemon to abort with `tokio-rt-worker has overflowed its stack`, taking bead authority and cluster status surfaces down. The immediate goal was to make the daemon runtime resilient enough for the large UI/status futures that run on busy authority nodes.

## Bead(s)

- `bd-9bb0b2` — v1.2.562 daemon stack overflow on tokio-rt-worker thread causing crash loop

## Before state

- Failing tests: none known locally at start of this slice; live Helsinki had repeated daemon aborts after v1.2.562 startup.
- Relevant metrics: Helsinki crash evidence showed repeated `tokio-rt-worker has overflowed its stack` within seconds of startup, followed by daemon unreachable / beads authority unavailable. Helsinki was rolled back to v1.2.561 to keep the board reachable.
- Context: the code used `tokio::runtime::Runtime::new()` for the main daemon and standalone beads daemon, leaving worker-thread stack size at Tokio/platform defaults while large monomorphized UI/status snapshot futures continued to grow.

## After state

- Failing tests: none in the focused validation run.
- Relevant metrics: `cargo test-small` passed; focused `cargo test -p caco-cli daemon_tokio_runtime_uses_explicit_large_stack_budget_bd_9bb0b2 --lib`, `cargo check -p caco-cli`, `cargo fmt --all -- --check`, and `git diff --check` passed before commit.
- Context: daemon runtime creation now uses a shared builder with explicit 16 MiB Tokio worker stacks for both `caco daemon` and `caco bd daemon serve`, matching the existing CLI dispatch stack budget.

## Diff summary

- Commits: the `bd-9bb0b2: increase daemon tokio worker stack` commit containing this summary (final SHA assigned by reintegration)
- Files touched: `crates/caco-cli/src/lib.rs`, `SPEC.md`, `README.md`, `AGENTS.md`, `docs/daemon.html`
- Tests: +1 focused unit test / -0 / flipped 0
- Behavioural delta: the daemon and standalone beads daemon no longer rely on default Tokio worker stack sizing, reducing the chance that large authority-node UI/status futures abort the entire daemon with a worker stack overflow. Operator/developer docs now record the explicit worker-stack contract.

## Operator-takeaway

The suspected Helsinki v1.2.562 crash path was not a data corruption issue; it was a daemon runtime stack-budget issue exposed by large async handlers on the authority node. This change makes the stack budget explicit and documented, but the fixed binary still needs to be deployed to Helsinki before `bd-a6b8aa` can be retried safely.
