# Session summary — cross-platform choice-resolution audit (bd-8b899d)

## Goal

Verify that the bd-355162 truncation root cause (webapp couldn't
resolve choices >100 newest old, ~14d in active projects) is or
isn't present on every other client surface that lists choices.
File or fix per surface as appropriate.

## Bead(s)

- `bd-8b899d` — Verify choice resolution works across other
  platforms (P2, task, choices/cross-platform/historical-data/
  testing). Direct sibling of bd-355162.

## Before state

bd-355162 fixed the webapp's `loadChoices` to pass `limit=1000`
explicitly so choices older than the daemon's silent 100-cap were
visible again. Audit gap: which other surfaces hit
`/api/v1/choices/list` and silently inherit the same daemon
default of 100?

## Audit matrix

| Surface | Endpoint touched | Status |
|---|---|---|
| caco-web | `/api/v1/choices/list` (loadChoices) | ✅ already fixed by bd-355162 (`limit=1000`) |
| **caco-cli** (`caco choices list`) | **`/api/v1/choices/list`** (dispatch_choices_list) | ❌ **BROKEN — same root cause**, fixed in this bead |
| caco-tui | `/api/v1/choices/active` (separate endpoint) | ✅ N/A — different code path, no truncation issue |
| iOS companion | only `POST /api/v1/choices/resolve` (resolves by ID) | ✅ N/A — never lists |
| iOS Watch | wears via iOS app | ✅ N/A |
| Android companion | only `POST /api/v1/choices/resolve` | ✅ N/A — never lists |
| Wear OS (Android) | uses `/choices/active` relay path | ✅ N/A — different endpoint |

Reproduction of CLI bug:

```
$ caco choices list --status all --json | jq '.count'
100                          ← silent cap
$ caco choices list --status all --limit 1000 --json | jq '.count'
149                          ← 49 silently dropped from default
```

## After state

CLI now extracts URL construction into `build_choices_list_url`
that defaults `--limit` to `1000` when omitted, mirroring the
webapp's bd-355162 mitigation:

```rust
fn build_choices_list_url(base_url, status, project, limit) -> String {
    let effective_status = status.unwrap_or("active");   // bd-2a4552
    let effective_limit  = limit.unwrap_or("1000");      // bd-8b899d
    ...
}
```

Verified live:

```
$ caco choices list --status all --json | jq '.count'
149                          ← full set surfaced
```

## Diff summary

- Files touched:
  - `crates/caco-cli/src/lib.rs`:
    - New `build_choices_list_url(base_url, status, project, limit)`
      helper extracted so the URL contract is unit-testable
      without exercising HTTP.
    - `dispatch_choices_list` delegates to the helper; default
      limit raised from "(unset → daemon default 100)" to
      "(unset → 1000 explicit)".
- Tests: +3 / -0
  - `build_choices_list_url_defaults_limit_to_1000_for_retention`
    pins the operator-facing default behaviour (limit=1000 +
    status=active).
  - `build_choices_list_url_explicit_limit_overrides_default`
    pins that an explicit `--limit 50` wins and the default does
    NOT also leak in.
  - `build_choices_list_url_optional_project_only_when_set` pins
    that `project=` only appears when caller passes a project.
- Test commands:
  - `cargo test -p caco-cli --lib build_choices_list_url` → 3 passed.
  - Live verify: `caco choices list --status all --json` returned
    149 (was 100 silent-truncated before).

## Out-of-scope follow-ups

Already filed — no new follow-ups needed:

- **bd-8031d0** (P3, filed during bd-355162): Apply the full
  bd-0b47a7 truncation-surfacing pattern to
  `/api/v1/choices/list` (`total_matched` / `truncated` / `limit`
  envelope fields + "showing N of M — load more" UI) and then
  revert both webapp and CLI's hardcoded `limit=1000`. This is
  the proper fix; the per-surface bumps land here as the
  matching one-shot mitigation.

## Operator-takeaway

`caco choices list --status all` (or `--status resolved`) now
returns the full retention window instead of the 100 newest. CLI
behaviour is now consistent with the webapp's bd-355162 fix.

Audit confirmed only TWO client surfaces ever hit the truncating
endpoint: webapp (fixed) and CLI (fixed). All mobile / TUI / Wear
surfaces use either `/choices/active` or `/choices/resolve` and
were never affected.

Honored constraints:
- `cargo test -p caco-cli --lib build_choices_list_url` only — no
  workspace test.
- Pre-close audit will run before close.
- Operator close-discipline: no out-of-scope work buried; the
  proper envelope-side fix is the existing bd-8031d0 follow-up.
- Operator `bd update --status=closed` bypass: ACK, using only
  `caco bd close`.
- Push-discipline (clarified): force-push only ever to my own
  agent branch, never to main/beads/shared. Confirmed.

24th bead closed this session (cumulative). 17th in this turn.
