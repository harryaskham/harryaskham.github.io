# Session summary — bd-78e6bc: extract_daemon_error_message swept across 28 dispatcher callsites

## Goal

Operator-takeaway sweep from bd-be23e4. Two patterns
leaked the raw {ok,error,meta} envelope at the user:

  format!("... failed ({status}): {body}")        — 17 sites
  format!("daemon returned {status}: {body}")     — 11 sites

(28 total in this sweep — original 2 callsites in
bd-be23e4 brought the total to 30 uses of the helper.)

## Bead(s)

- `bd-78e6bc` — own follow-up. Closed.

## Before state

```
$ caco agent discard --id nonexistent
error: daemon returned 404: {"ok":false,"error":
{"code":"discard_failed","message":"daemon error:
agent not found: nonexistent"},"meta":{"request_id":
"req-..."}}

$ caco agent stop --id nonexistent           (similar)
```

## After state

```
$ caco agent discard --id nonexistent
error: daemon error: agent not found: nonexistent

$ caco agent stop --id nonexistent
error: daemon error: agent not found: nonexistent
```

## Diff summary

- 1 file touched, ~85 lines changed (28 sites × 3
  lines each ≈ 84):
  - `crates/caco-cli/src/lib.rs`:
    - 11 simple `daemon returned {status}: {body}`
      sites (sed-replaced, mechanical).
    - 17 `<phrase> failed ({status}): {body}` sites
      (regex-rewritten, mechanical).
    - All sites converged on the shape:
      ```
      let pretty = extract_daemon_error_message(&body)
          .unwrap_or_else(|| format!(<original-fallback>));
      Err(CliError::new(pretty))
      ```

## Verification

- `cargo build --bin caco`: clean.
- `cargo test-small`: 57 passed.
- `cargo test -p caco-cli --lib agent_introspect`:
  3 passed (sample of the cluster).
- 4 cases verified live: agent discard / stop / nudge
  / bd claim — all 4 surface the friendly daemon
  message instead of the raw envelope.

## Operator-takeaway

The `extract_daemon_error_message` helper has reached
the saturation point: 30 callsites use it, all
following the same fallback shape. New non-bd
dispatchers should reach for it by default — the
helper is cheap (single JSON-decode + lookup) and
gracefully no-ops on non-JSON bodies.

The sweep itself was a useful demonstration of "use
the smallest tool that works":
- Pattern A (string-literal): plain str.replace, 11
  matches.  Trivially safe.
- Pattern B (multiline format!): tiny Python regex,
  17 matches.  Compiled with cargo on first try; no
  manual touch-ups needed.

When a sweep gets to 5+ uniform sites, scripted is
faster than per-site manual edits AND less error-
prone.  Below 5, manual is fine.

The remaining `daemon request failed: error sending
request` surfaces (transport failure path) are NOT
in this family — they're reqwest::Error not HTTP
4xx/5xx envelopes, so extract_daemon_error_message
wouldn't help.  Those need a separate "is the
daemon actually up?" precheck story (related to
bd-be23e4 issue 2 deferral).

Cluster status:
- silent-unknown-value family: 15 instances closed.
- silent-coercion family: 9 sites + 4 helpers landed.
- raw-JSON-envelope-leak: 30 callsites adopted helper.
