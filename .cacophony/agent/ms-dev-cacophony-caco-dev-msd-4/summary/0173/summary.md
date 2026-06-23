# Session summary — Fix token_stats parser for real Pi v3 session shape (bd-b18111 stage A.5)

## Goal

Continue bd-2da2c5 slice 2. While verifying the bd-9b88a4 token foundation
before reusing it in the daemon collector (the bead's explicit
VERIFY-BEFORE-IMPLEMENT directive), I found the parser was tested against a
synthetic shape that does not match real Pi v3 session logs, so `caco node
tokens` reported zero. This chunk fixes the parser so it works on real data —
the necessary correctness foundation before the daemon collection loop reuses it.

## Bead(s)

- `bd-b18111` — [bd-2da2c5 s2] Daemon+agents: token-usage emission + collection sink + per-project aggregation
- (parent epic: `bd-2da2c5` — unified caco stats; foundation: `bd-9b88a4`)

## Before state

- Failing tests: none (but the 8 existing tests only exercised a synthetic flat shape).
- `caco node tokens --json` on this node returned `projects: {}`, `grand_total: 0`
  despite ~24B real tokens in `~/.pi/agent/sessions`.
- Root cause: `parse_pi_session_usage_line` read top-level `usage`/`cwd`/`model`
  per line, but real Pi v3 sessions nest `usage`/`model` under a `message`
  object and carry `cwd` only on the leading `type:"session"` header line.

## After state

- Failing tests: none. 11/11 caco-stats unit tests pass (3 new real-v3-shape tests).
- `parse_pi_session_usage_line` reads `.message.usage` / `.message.model` with a
  top-level fallback (legacy/synthetic flat shape still works).
- `read_pi_session_token_usage` resolves each file's project once from the
  session-header cwd and applies it to usage turns lacking their own cwd, while
  still honoring a per-line cwd when present (mixed/legacy files attribute per-line).
- Verified against this node's real sessions: cacophony ~24.0B, plus picasso,
  collective, omni-cli, life — where the old parser produced zero.

## Diff summary

- Code/content commits: `cffa2f7e7e` (pending final squash SHA from reintegration receipt)
- Summary artefact commit: intentionally omitted (no self-reference)
- Files touched: `crates/caco-stats/src/token_stats.rs` (parser + reader + module doc + 3 tests)
- Tests: +3 / -0 / flipped 0
- Behavioural delta: `caco node tokens` now reports real per-project token totals
  instead of all-zero; same fixed reader is what the daemon collector will reuse.

## Operator-takeaway

The token foundation looked done but silently produced zero on real Pi v3
sessions because it was only tested against a synthetic flat shape — a good
reminder that telemetry parsers need real-fixture tests. With this fix `caco
node tokens` works, and the daemon collection loop (next chunk) can rely on the
reader to actually emit rows. Next: the daemon-side append-only token-usage sink
+ SQLite index + periodic collection loop reusing this reader.
