# bd-5993ed — Vague-bead audit report

Audit run: 2026-04-23 by msm-1 against the live daemon snapshot
(163 beads in {open, in_progress, blocked}).

## Methodology

Pulled all open/in_progress/blocked beads via `caco bd show --json`
in parallel; ran heuristics for:

1. `<UNKNOWN>` literal in `dependencies`
2. Non-bd-id strings in `dependencies` (LLM-generated names that
   never got resolved to real IDs by the bead-expand flow)
3. Run-on titles with empty descriptions
4. Short / vague titles (≤4 words, missing context)
5. Duplicate title pairs

## Findings

### Bad-dep cleanup (9 beads, 13 bad refs resolved)

| Bead | Bad dep | Resolution |
|---|---|---|
| bd-d21634 | `<UNKNOWN>` | dropped |
| bd-2506fe | `review-tts-source` | → bd-fbc9e9 |
| bd-b39bdc | `design-bead-surface` | → bd-2a3aeb |
| bd-45afb3 | `design-bead-surface` | → bd-2a3aeb |
| bd-555682 | `research-private-distribution` | → bd-8299cc |
| bd-c77af9 | `research-private-distribution` | → bd-8299cc |
| bd-7a8e17 | `Research private Android app distribution options` | → bd-8299cc |
| bd-cf3183 | 3 timeline strings | → bd-47dc20, bd-5278e2, bd-fc8947 |
| bd-96d69d | `Implement timeline data generation service` | dropped (no match) |

After: bd-d21634 + bd-96d69d unblocked. The rest now have resolvable
real-bd-id dependency chains and will deblock when their parents close.

### Duplicate close

- bd-d4d907 (younger, less detailed) → marked duplicate of bd-8299cc
  (kept canonical, retains explicit option-list).

### Restructured run-on title + empty desc

- bd-a7168d: title was a 200-char sentence; description empty.
  Restructured into a proper title plus description with symptom /
  proposal / acceptance criteria. Bead remains assigned to its
  current claimant (cacophony:7wtvqfrmubjddjdq) — only metadata
  cleanup, no reassignment.

### Verified clean (false positives)

Short titles that turned out to have full descriptions:

- bd-5cf9a5 (Create enterprise.yaml theme file) — 689ch desc
- bd-11bc46 (Validate transcription functionality) — 239ch desc
- bd-c77af9 (Implement automated update distribution) — 386ch desc
- bd-c8e045 (Update build instructions for macOS development) — 229ch desc
- bd-b45b04 (Update theme configuration to register enterprise.yaml) — 546ch desc

These pass the "well-scoped" bar; no action.

### Empty-label inventory (informational)

~46 beads have empty `labels`. Most are EPIC/permanent/STT/voice
beads that carry their tag as a `[bracket-prefix]` in the title
instead of as a label. Not flagged as problematic; future label
backfill could be a separate process-improvement bead.

## Follow-ups

- bd-d21634 is now unblocked and claimable. It calls for "bead
  submission guidelines + validation" — partially addressed by
  this audit's heuristics, which could be lifted into a daemon-side
  validator. Leaving for the next claimant of bd-d21634.

## Acceptance criteria status

- [x] Audit of all open beads completed (163)
- [x] Problematic beads categorized (4 categories)
- [x] Action taken on each (10 updates, 1 dup-close, 1 restructure)
- [x] Project backlog is clean and well-scoped (no remaining
      `<UNKNOWN>` deps, no remaining string-literal deps)
