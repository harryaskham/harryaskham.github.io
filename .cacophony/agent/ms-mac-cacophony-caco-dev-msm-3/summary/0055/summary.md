# Session summary — bd-07cbd9 caco ssh flag-arg adjacency

## Goal

Fix caco ssh / scp pass-through corrupting commands that pair a short
flag with its argument (-L 8080:localhost:8080, -i /path/to/key, -o
KEY=val, etc.). Reported by test-user pass on cacophony 1.2.515.

## Bead(s)

- `bd-07cbd9` — caco ssh pass-through reorders flags + breaks
  flag-with-argument forms

## Before state

- 'caco ssh --print helsinki -L 8080:localhost:8080 cmd'
   → 'ssh ... cmd -L'  (broken: flag separated from argument)
- All ssh/scp short flags taking arguments were affected

## After state

- Parser knows the OpenSSH/Scp flag-with-arg short-option set and
  eagerly consumes the next token adjacent to such flags
- Trailing positionals in passthrough commands route through
  passthrough_args so original token order survives the
  positionals/passthrough split
- Hand-verified six representative invocations (-L, -t, -X,
  combined -i + -L, -o KEY=val, scp -P port-override)

## Diff summary

- Commits: 307365e3c954
- Files: `crates/caco-cli/src/lib.rs` (+129 -12 incl. tests),
  `crates/caco-web/src/tests.rs` (drive-by clippy fix for
  bd-e49551's keyboard test)
- Tests: 2 updated (corrected post-fix expectations), +3 new

## Operator-takeaway

Same root cause shape as bd-125b70 (caco scp --print drops
positionals): the parser's positional / passthrough split was
order-destroying. Generalised the fix instead of patching scp again.
Worth a doc note on the passthrough-command pattern: dispatchers
should treat positionals[0] + passthrough_args as a single
order-preserved arg stream.
