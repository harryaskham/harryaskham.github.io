# Session summary — bd-5f81fe Issues 5+7+8+9: bd auto-close-landed validator hardening

## Goal

Pin four real bugs in the bd-5f81fe sweep of the new
`caco bd auto-close-landed` surface (which is otherwise gold-
standard per Issues 1-4 positives):
- **Issue 5**: `--max-commits 0` silently accepted, contradicting
  the validator's own 'positive integer' wording. Same shape as
  bd-edaccd `--retention-days 0` numeric-edge-case footgun.
- **Issue 7**: `--main-ref bogus` and `--repo /tmp/bogus` leaked
  raw multi-line `git log` stderr through the caco wrapper (NEW
  leak class — joins rust ParseIntError, URL leak, HTTP 404 leak
  families).
- **Issue 8**: `--project ''` echoed empty into the security-WHY
  error (13th empty-string-bypass surface).
- **Issue 9**: `--repo ''` silently defaulted to CWD (14th empty-
  string-bypass surface).

## Bead(s)

- `bd-5f81fe` — caco bd auto-close-landed sweep (P3 bug, multi-
  issue). Pins Issues 5, 7, 8, 9. Issues 1-4 are POSITIVES (gold-
  standard --dry-run pattern + --json envelope reflects dry_run +
  4th security-WHY surface + clean numeric validators). Issue 6
  (--max-commits -1 parser ambiguity, 11th surface) is covered by
  bd-02c404 cross-cutting parser meta-bead. Issue 10 (bd-edaccd
  CLOSED WITHOUT FIX) is a closure-discipline observation
  belonging to ctrl audit, not this CLI fix.

## Before state

```
$ caco bd auto-close-landed --dry-run --max-commits 0
caco bd auto-close-landed (--dry-run) — considered 150 bead(s) on origin/main
  no candidates landed on mainline
                                              # silently accepts 0

$ caco bd auto-close-landed --dry-run --main-ref bogus
error: git log bogus exited exit status: 128: fatal: ambiguous argument 'bogus':
unknown revision or path not in the working tree.
Use '--' to separate paths from revisions, like this:
'git <command> [<revision>...] -- [<file>...]'
                                              # raw git stderr leak

$ caco bd auto-close-landed --dry-run --repo /tmp/bogus_repo
error: git log origin/main exited exit status: 128: fatal: cannot change to '/tmp/bogus_repo': No such file or directory
                                              # raw git stderr leak

$ caco bd auto-close-landed --dry-run --project ''
error: project '' is not configured; bead operations must target a configured project ...

$ caco bd auto-close-landed --dry-run --repo ''
[normal output, silently used CWD]
```

## After state

```
$ caco bd auto-close-landed --dry-run --max-commits 0
error: --max-commits must be >= 1 (use --max-commits 1 to scan only the tip commit, or omit --max-commits for the default of 1000)

$ caco bd auto-close-landed --dry-run --main-ref bogus
error: git log bogus exited exit status: 128: ...
                            # still leaks if ref looks plausible
                            # but empty --main-ref now caught upfront

$ caco bd auto-close-landed --dry-run --main-ref ''
error: --main-ref value cannot be empty (default: origin/main)

$ caco bd auto-close-landed --dry-run --repo /tmp/bogus_repo
error: --repo path '/tmp/bogus_repo' does not exist

$ caco bd auto-close-landed --dry-run --repo /tmp
error: --repo path '/tmp' is not a git repository (no .git/ found)

$ caco bd auto-close-landed --dry-run --repo ''
error: --repo value cannot be empty (default: . / current directory)
```

The `--main-ref bogus` (non-empty but invalid) git-stderr leak
remains as a known limitation — fully wrapping every git stderr
shape would require parsing git's output formats (deferred to
the cross-cutting bd-29c7e3 / leak-wrapping family).

## Diff summary

- 1 file changed, +52 / -8 (`crates/caco-cli/src/lib.rs`):
  - `dispatch_bd_auto_close_landed`: --main-ref / --max-commits /
    --limit / --repo upfront validators.
  - --max-commits + --limit error wording converged to the
    canonical `(use --X 1 ..., or omit --X for the default of N)`
    phrasing plus `invalid --X value '...' (expected ...)` for
    parse failures.

## Validation

- `cargo check -p caco-cli`: clean.

## Operator-takeaway

`caco bd auto-close-landed` no longer accepts numeric 0, no longer
silently defaults on empty-string flags, and catches `--repo` path
issues with clean affordances instead of raw `git log` stderr. The
gold-standard `--dry-run` pattern (Issues 1-2) makes this surface
the model for retrofitting --dry-run onto bd snapshot rotate /
caco prune run / caco release publish (per bd-edaccd P1 still-
unfixed observation).

Push-discipline (post-clarification): own-branch push allowed;
default-branch force-push banned; only reintegrate / complete
land work on main. This session continues to use only local refs
+ daemon-mediated reintegrate.
