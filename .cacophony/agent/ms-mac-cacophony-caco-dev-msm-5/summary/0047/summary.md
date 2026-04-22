# Session summary 0047 — bd-6b7b30: operator-actions list (slice 1)

## Goal

Centralized surface for pending operator-required tasks across
the fleet, so beads like bd-828c12 (sops-nix key deploy) don't
sit blocked >5h waiting for the operator to notice.

## Bead(s)

- `bd-6b7b30` slice 1 — read-only aggregator only.

## Before state

- No discoverable command for "what's blocked on me?".
- bd-828c12 sat for 5+ hours with no ambient signal.

## After state

- `caco operator-actions list [--project NAME] [--json]`.
- Re-shells `caco bd list --status open --json` and filters to
  titles containing `[operator-action]` or starting with
  `operator-action:`.
- Sorted by `created_at` ascending so the stalest blockers
  bubble up first.
- Pretty-print: id, priority, created_at, title; footer points
  at slice 2 (per-action ack workflow).
- `--json` returns `{ok, data: {project, count, actions[]}}`
  envelope.

## Diff summary

- Commit: `d7ee7224`.
- Files (1): caco-cli lib.rs (+110 lines).
- `cargo build` and `cargo clippy`: clean.

## Operator-takeaway

Run `caco operator-actions list` to see what's blocked on you,
oldest first. Slice 2 (per-action UX: `caco secret rotate --node
N --key K` queue + `caco secret apply` ack on the right machine,
pre-shared ed25519 generation, per-node grouping) will graduate
this from a list to a workflow. Filed when demand surfaces.
