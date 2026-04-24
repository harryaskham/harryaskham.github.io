# Session summary — bd-b7392e caco outbox UX hardening

## Goal

Apply the gold-standard `caco node` agent-context UX pattern to
`caco outbox` so workers don't get tempted into dead-end subcommands
they could never run.

## Bead(s)

- `bd-b7392e` — caco outbox is fully blocked for worker scope but
  EXPOSES all 5 subcommands in --help and only auth-rejects at
  execution time with cryptic 'worker scope cannot access /api/v1/outbox'

## Before state

- `caco outbox --help` (as worker) listed all five operational
  subcommands (list/show/retry/flush/drop) plus mcp.
- Running any of them returned the cryptic
  `worker scope cannot access /api/v1/outbox` (information leak about
  internal API path; no hint about how to escalate).
- Compare gold standard `caco node`: hides `join` from worker --help
  with `(note: 1 additional subcommand(s) hidden in agent context:
  join; run from an operator shell to see them)`.

## After state

- All five operational subcommands now `agent_safe = false`. The
  existing `render_text_help` filter automatically hides them and
  appends the standard `(note: 5 additional subcommand(s) hidden in
  agent context: list, show, retry, flush, drop; run from an operator
  shell to see them)` line.
- `mcp` branch stays visible (it's the discovery surface).
- Daemon `scope_allows()` now has a special-case branch BEFORE the
  catch-all deny: `/api/v1/outbox*` from worker scope returns
  `caco outbox requires operator scope; run from an operator shell
  or via `caco mode promote-scope` (bd-b7392e)` — no internal path
  leak, explicit escalation hint.

## Diff summary

- 2 files modified, 66 insertions, 5 deletions:
  - `crates/caco-cli/src/lib.rs`: OUTBOX_SUBCOMMANDS flag flips +
    rationale comment + 1 new test.
  - `crates/caco-daemon/src/lib.rs`: special-case branch in
    `scope_allows()` for `/api/v1/outbox*`.
- Tests: cargo test-small 175 passed; new
  `caco_outbox_subcommands_are_hidden_in_agent_context` exercises
  the agent_safe flag for all five operational subcommands plus
  preserves the mcp branch's visibility.

## Out of scope (deferred per bead description)

- Issue 2 (validators short-circuit by auth check): bead confirms
  this is correct security; the suggested doc-in-help for ranges is
  cosmetic.
- Issue 3 (error envelope): bead confirms `{ok, error, meta}` on
  failure mutually exclusive with `{ok, data, meta}` on success is
  already correct.

## Operator-takeaway

Three-layer UX win matches the bead description: (a) worker doesn't
get tempted into a dead end (subcommands gone from their --help),
(b) worker who suspects there's more is told the magic words,
(c) gated subcommand isn't an information leak about internal
architecture. This pattern is now applied consistently across `caco
node` (join) and `caco outbox` (list/show/retry/flush/drop) — likely
a similar fix exists for other operator-only subcommand families
worth a sweep in a follow-up bead.
