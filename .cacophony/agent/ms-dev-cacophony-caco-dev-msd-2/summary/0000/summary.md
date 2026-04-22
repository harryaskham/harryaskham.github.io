# Session summary — narrator flat-window minimal output

## Goal

Reduce narrator cognitive load. Tonight's narrator across the
02:xx-04:xx BST window emitted ~10 multi-paragraph speak messages,
most of which contained `fleet UNCHANGED` or `completed STILL flat`.
Important context but pure noise on flat sweeps. The narrator profile
should distinguish *event* from *sweep with no events* and produce
minimal output in the latter case.

## Bead(s)

- `bd-58ff27` — Narrator should distinguish 'event' vs 'sweep with no
  events' so flat windows produce minimal speak output

## Before state

- `.cacophony/profiles/narrator.md` Idle Behavior section said only:
  "Emit a brief 'all quiet' acknowledgement via `caco msg speak` and
  wait for the next nudge cycle." No concrete definition of what
  counts as quiet, no output-discipline contract, no incident-bypass
  rule. In practice agents under this profile produced full
  multi-paragraph narration every cycle.
- No tests cover narrator profile content (it is prose), but
  `cargo test -p caco-config --lib profile` exercises the YAML
  frontmatter parse and `caco config validate` round-trips the file.

## After state

- New `### Flat-window minimal output (bd-58ff27)` subsection added
  inside `## Idle Behavior` of `.cacophony/profiles/narrator.md`.
- The subsection defines:
  1. **A 5-criterion 'is this sweep flat?' heuristic** the narrator
     compares against its own previous narration: terminal-state
     counts moved, bead status changes, release/version bump,
     notable node-health transition, directed operator inbox message.
  2. **Output discipline by sweep kind**:
     - flat → single ~25-word tick line (`"Tick HH:MM — fleet flat:
       N agents idle, M in-flight, queue at K."`),
     - eventful → existing multi-paragraph form,
     - **incident → always full narration regardless of
       classification** so flat-window suppression cannot suppress
       failures, beads-primary outages, or stuck workers.
  3. A '"still / unchanged / continues to" used 3+ times' anti-pattern
     detector as a heuristic that the agent is over-budget for a
     flat tick and should collapse further.
  4. Anchors against the agent's own previous `caco msg speak`
     outputs as the comparison baseline (re-read recent speaks /
     narrator inbox entries before composing).
- All other narrator behaviour (responsibilities, control surfaces,
  node-health noise calibration, completion rules, etc.) preserved.

## Files touched

- `.cacophony/profiles/narrator.md` (+48 lines)

## Diff summary

Single-file profile-prose change. `.cacophony/profiles/narrator.md`
gains a new `### Flat-window minimal output (bd-58ff27)` subsection
under `## Idle Behavior` (+48 lines, no deletions). The new
subsection codifies a 5-criterion 'is the sweep flat?' heuristic, an
output-discipline contract per sweep kind (flat / eventful /
incident), an explicit incident-bypass clause so failures and
beads-primary outages are never suppressed by flat-window logic, and
an anti-pattern detector keyed on repeated 'still / unchanged /
continues to' phrasing. No frontmatter changes; no Rust changes; no
schema changes.

## Operator-takeaway

Narrators running this profile should now emit a single short tick
line (~25 words) on flat sweeps and reserve full multi-paragraph
narration for cycles where at least one of {terminal-state count
moved, bead status changed, release landed, notable node-health
transition, directed inbox message} occurred. Failures, stuck
workers, and beads-primary outages always bypass the flat-window
suppression. Watch the next ~24 hours of narrator output — if flat
sweeps still produce paragraphs, the profile prose may need to be
replaced or supplemented by a programmatic state-hash check (called
out as a follow-up below). No worker beads change behaviour; only
agents launched under the `narrator` profile are affected.

## Validation

- `cargo test -p caco-config --lib profile` → 13/13 PASS (frontmatter
  parse exercised across the profile-loading suite).
- `caco config validate` → "config valid" (8 nodes, 7 projects). This
  loads and merges the modified narrator profile end-to-end, so a
  YAML break in the new subsection's containing frontmatter would
  surface here. (The new content is body-only, not frontmatter.)
- `cargo test-small` → 716+286+18+2798+45 PASS green (run earlier in
  session under bd-eef5da; nothing in this commit touches Rust).
- Did not run full workspace tests per merge-queue mixin and the
  zero-Rust-diff scope.

## Notes / follow-ups

- Pure profile-prompt change: behavioural improvement depends on
  the narrator runtime actually following the new output contract.
  Worth a 24-hour observation window — if narrators still emit full
  paragraphs on flat sweeps, the heuristic may need tightening or
  the runtime may need a programmatic delta check rather than a
  profile-prose nudge.
- The bead's title mentions "tracking last-spoken state hash". This
  patch implements the *behavioural* equivalent (the agent re-reads
  its own prior speaks) rather than a programmatic state-hash
  primitive. A follow-up could add a `narrator_state_hash` MCP tool
  if prose discipline proves insufficient.
- Earlier in this session: bd-eef5da (short_name profile fallback)
  was filed and patched but obsoleted mid-session by parallel work
  from ms-mac ephemeral `mab6fpzek734d79t` landing the same
  resolve_short_name → AdjNoun fallback under bd-34d0b8 (commit
  0ed3aa48). My patch was dropped, the bead was annotated with the
  remaining unimplemented (1) scope (profile_strategy bridge-output
  plumbing at both spawn callsites), and the branch was force-pushed
  back to clean.
