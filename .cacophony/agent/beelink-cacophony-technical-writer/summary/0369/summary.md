# Technical-writer summary — fix scrapeable garbage commit-subject template in profile

## Goal

Fix the root cause of the broken hourly-review commit subjects landing on main
(flagged by msd-0, tracked by bd-6f31f1). My reintegrations have been landing with
the literal subject `bd-1d2e41: hourly review — update X, refresh gh-pages Y` plus a
stray trailing backtick — unsubstituted `X`/`Y` placeholders that pollute git
log/blame on ~10 of the last 40 commits (including the cacheless-docs merge
49d68729b). Root cause: the reintegration squash-subject path scrapes a literal
example commit subject verbatim from this profile (the bd-a25dc2 daemon class), and
`.cacophony/profiles/technical-writer.md` shipped exactly such an example.

## Bead(s)

- bd-6f31f1 (P3 draft, msd-0): the in-my-lane template fix. Related daemon root
  cause: bd-a25dc2 (squash-subject scrapes profile example instead of the actual
  commit message) — separate dev lane.

## Before state

- `.cacophony/profiles/technical-writer.md` Step 6 item 2 said: "commit it with a
  message like `bd-1d2e41: hourly review — update X, refresh gh-pages Y`." The daemon
  scraped that bead-prefixed literal (markdown backtick included) as the squash
  subject for every hourly-review reintegration.

## After state

- That item now gives prose guidance: commit with a concise, descriptive `docs:`-
  prefixed subject naming the actual surfaces changed; do not use a generic
  placeholder, unsubstituted `X`/`Y` tokens, or a literal example subject copied from
  the profile — because the squash-subject path can scrape a profile example verbatim
  onto main (cross-refs bd-a25dc2 / bd-6f31f1). No literal `bd-`-prefixed commit
  subject remains in the profile for the daemon to scrape.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `.cacophony/profiles/technical-writer.md` (Step 6 item 2 prose).
- Tests: n/a (profile prose; docs-only lane, self-configuration landed via
  reintegration per AGENTS.md).
- Behavioural delta: none to code; removes the scrapeable garbage commit-subject
  source.

## Operator-takeaway

The garbage `update X, refresh gh-pages Y` commit-subject source is removed from the
technical-writer profile. Caveats: (1) full effect for the LIVE
beelink-cacophony-technical-writer runtime needs a profile refresh / `caco agent
recreate` (the reified profile still carries the old example until then), and (2) the
deeper daemon behavior of scraping profile examples instead of the actual commit
message remains tracked by bd-a25dc2. This reintegration itself may still land with
the old scraped subject for the same reason.
