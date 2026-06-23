# Session summary — Lift ms-mac avoid-queued-cargo caveat (bd-109ab3 / bd-880d4c)

## Goal

Lift the avoid-queued-cargo-on-ms-mac caveat in AGENTS.md now that bd-880d4c
is CLOSED (ms-mac's eval-cache / in-memory redhill resolved), so the canonical
agent doc stops telling agents to avoid ms-mac queued cargo when it is safe.

## Bead(s)

- `bd-109ab3` — reconcile Nix-cache docs (AGENTS.md slice; technical-writer owns README/deploy).
- `bd-880d4c` — ms-mac eval-cache saga (CLOSED by ms-mac-ctrl/msm-1).

## Before state

- AGENTS.md L191/L233 said the ms-mac override + avoid-queued-cargo caveat are
  KEPT until the eval-cache refresh is confirmed (ctrl condition 3).

## After state

- L191/L233 now state ms-mac's in-memory redhill was cleared by an operator
  nix-daemon restart, both layers confirmed clean (bd-880d4c CLOSED), the
  caveat is LIFTED, bd-716f8e inherit-host is safe on ms-mac (no pin), and
  override-owners can drop overrides (bd-b4cc45 lane).

## Diff summary

- Files touched: AGENTS.md (L191 + L233 ms-mac caveat sentences).
- Code/content commit: pending final landed SHA from the reintegration receipt.
- Behavioural delta: docs-only; releases condition-3 once the eval-cache
  confirm landed (msm-1's override-less verify).

## Operator-takeaway

The whole ms-mac caveat lifecycle (restore in b99470bce, lift here) was gated
correctly on the empirical eval-cache confirm — the doc never said ms-mac
queued cargo was safe before msm-1's override-less run proved it.
