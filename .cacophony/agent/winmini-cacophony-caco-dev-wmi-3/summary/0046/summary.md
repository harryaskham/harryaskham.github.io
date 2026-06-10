# bd-d4d42e — Outbox bead_create idempotency (eliminate the retry duplicate-create storm)

## Bead
bd-d4d42e (beads/daemon-resilience/idempotency/outbox, P2) — caco-ctrl-filed. Root cause of po4-3's recurring phantom watchtower bead: a stuck-pending outbox `bead_create` re-created a NEW duplicate bead on each retry.

## Root cause (refined)
The lost-response problem: the beads primary created the bead, but the create response was lost (transport drop), so the outbox entry stayed `pending` and each subsequent flush re-POSTed → another dup. The store's existing duplicate check (bd-b691f6) only catches **open** beads, so once po4-3 closed each dup, the next retry escaped the check and created a fresh one. `bd-1f04d8` already ages out creates stuck >24h, but within the window the retry still dups.

## Change (server-side idempotency keyed on the stable outbox entry id)
### crates/caco-beads/src/store.rs
- New `bead_idempotency(idempotency_key PRIMARY KEY, bead_id, created_at)` table (additive migration).
- `create_bead_idempotent(key, params, actor, force)`: if the key was already used, return the original bead — **even if it has since been closed** (via `get_bead`, falling through to a normal create only if the mapped bead was hard-deleted); otherwise create (`create_bead_with_force`) and record the key→bead_id mapping (`INSERT OR IGNORE`). Empty key → ordinary create. The existing `create_bead`/`create_bead_inner`/`create_bead_with_force` paths are byte-unchanged (zero blast radius when no key).

### crates/caco-daemon/src/beads.rs
- `handle_create_bead` (local primary path) reads the `x-caco-idempotency-key` header; when present it routes to `create_bead_idempotent`, otherwise the existing `create_bead_with_force`. Additive branch.

### crates/caco-daemon/src/outbox.rs
- `attempt_delivery` sends `x-caco-idempotency-key: <entry.id>` for `bead_create` (the entry id is stable across retries), so every outbox retry of the same create maps to the same bead.

## Effect
Outbox retries of a lost-response create are now idempotent: the first retry creates + records the key; all later retries return the original bead (mark delivered, no dup), regardless of whether it was closed in between. This eliminates the recurring-phantom storm.

## Scope / follow-ups (filed separately)
- The very first cluster-forward attempt (in `forward_post_with_outbox`, before an outbox entry exists) does not yet carry the key, so a lost response on the first attempt can still dup ONCE before the idempotent retries kick in. Threading a key through the first forward is a clean follow-up.
- The secondary finding (5146 stale **failed** outbox entries / retention pruning) is explicitly a separate outbox-retention pass per the bead.

## Validation (daemon test queue, --cwd at checkout)
- `cargo test -p caco-beads --lib d4d42e` (tj-eb1a9bea): PASSED — `create_bead_idempotent_dedupes_by_key_bd_d4d42e` (same key returns original even after close; new key creates; empty key ordinary create).
- `cargo clippy -p caco-beads --lib` (tj-36754752): PASSED, 0 warnings.
- `cargo clippy -p caco-daemon --lib` (tj-9ac5af3e): PASSED, 0 warnings (compiles the beads.rs + outbox.rs changes).
- All changed regions rustfmt-clean (skip_children reformat diff); `git diff --check` clean.

## Diff
See the reintegration receipt for the landed squash SHA.
