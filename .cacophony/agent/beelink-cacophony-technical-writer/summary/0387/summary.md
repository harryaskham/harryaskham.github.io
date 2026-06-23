# Technical-writer review summary

## Goal

Document the new gate-slot reservation mechanism (bd-a54742) in the AGENTS.md
gate-admission section.

## Bead(s)

- `bd-c63005` — technical-writer documentation maintenance.
- Documents bd-a54742 (reserve gate slots by capping normal-job concurrency).

## Before state

- AGENTS.md's gate-admission text ended at "a gate job never preempts an already-running normal job," with no mention of the new optional gate-slot reservation.

## After state

- AGENTS.md now documents `CACO_TEST_QUEUE_GATE_RESERVED_SLOTS` (default `0` = off): a node can reserve gate slots by capping concurrent normal jobs to `node_max_parallel - reserved`, so a gate runs alongside running worker jobs instead of stalling behind them during a compile storm — staying within the node ceiling (no oversubscription/OOM, unlike preemption), only on multi-slot nodes, never reserving the last slot.
- Verified against `crates/caco-daemon/src/test_queue.rs` (`gate_reserved_slots` / `normal_concurrency_cap`, bd-a54742). Only AGENTS.md carries the detailed gate-admission text, so the addition is scoped there.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `AGENTS.md`.
- Behavioural delta: documentation only.

## Operator-takeaway

The gate-admission docs now cover the optional slot reservation, so operators can
tune `CACO_TEST_QUEUE_GATE_RESERVED_SLOTS` to keep reintegration gates moving on
busy multi-slot nodes without oversubscribing.
