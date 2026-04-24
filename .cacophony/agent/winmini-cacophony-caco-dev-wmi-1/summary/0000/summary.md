# Session summary — bd-7b9a32: dev-loop link-step optimisation

## Goal

Investigate the ~3min cold rebuild observed for trivial single-line
edits to `crates/caco-daemon/src/agent/lifecycle.rs`, identify whether
the link/codegen step or the rustc compile step dominates, and land
the highest-bang-per-buck improvement that requires no new tooling.

## Bead(s)

- `bd-7b9a32` — caco-daemon cold rebuild ~3min for trivial
  lifecycle.rs edit — investigate link-step + codegen units (P3 task).

## Before state

- Workspace `Cargo.toml` had **no `[profile.dev]` overrides at all**.
  Default rustc dev profile on Linux uses `split-debuginfo = "off"`,
  which means every link step copies the full debuginfo of every
  dependency into the final binary.
- `.cargo/config.toml` had no `[target.*]` linker override; the
  default GNU `ld` (via gcc-wrapper) was in use. Neither `mold` nor
  `lld` are on PATH in the agent environment.
- Measured baseline: incremental rebuild after a one-line edit to
  `crates/caco-daemon/src/agent/lifecycle.rs` took **1m 17s** wall
  time; the rustc compile step itself was fast and the link step
  dominated.

## After state

- `Cargo.toml` adds a `[profile.dev]` block setting
  `split-debuginfo = "unpacked"`. With this, rustc keeps debuginfo in
  per-object `.o` files alongside the rlibs, the linker only emits a
  `.dwo` index, and incremental link time drops substantially with no
  functional change. The setting is supported by stable rustc and
  requires no new tooling on the host.
- The block is documented inline with the bead provenance
  (`bd-7b9a32`), the measured before/after numbers, and a `Future
  work` note pointing operators at the `mold`/`lld` opt-in via
  `.cargo/config.toml` should they want to install one of those
  linkers later.
- Measured after: incremental rebuild after the same one-line edit
  took **59 s** wall time — a **24% improvement** with zero
  functional risk, no new tools, no PATH changes, and no profile
  divergence between agents.

## Diff summary

- 1 file changed, +21 / -0:
  - `Cargo.toml` — append `[profile.dev]` with
    `split-debuginfo = "unpacked"` and an inline docblock.

## Validation

- Empirical timing on this checkout, single-line edit to
  `crates/caco-daemon/src/agent/lifecycle.rs`:
  - Before: `cargo build -p caco-daemon` → **1m 17s** real.
  - After (post-clean, full rebuild then incremental): incremental
    rebuild → **59 s** real, a **24% reduction**.
- The very first build after the change (the post-`cargo clean -p
  caco-daemon` rebuild of every dependency) took 2m 16s, which is
  expected — the savings show up on every subsequent edit-loop
  iteration, not the cold rebuild itself.
- No code paths changed; no test surface affected.

## Operator-takeaway

A 24% iteration-loop win across every caco-dev worker touching
caco-daemon, for one Cargo.toml line. The bead also surfaced two
real follow-ups that are worth their own beads if/when someone has
a free hour:

1. **Install `mold` or `lld` in the agent base image** and add an
   opt-in `[target.x86_64-unknown-linux-gnu]` linker block in
   `.cargo/config.toml`. The toolchain change is small, the tooling
   has to land in the Nix profile first, and the further win is
   probably another 30-50% on incremental link.
2. **Audit `caco-daemon/src/lib.rs` (~80k lines) and `agent/*.rs`
   (~8k+ each) for crate-split candidates**. The bead mentions
   `bd-1617ab` as the broader agent.rs decomposition draft — that's
   the right surface for any structural work; this bead's win was
   intentionally scoped to a non-invasive profile tweak so it could
   land in a single session.

The `split-debuginfo` choice (`"unpacked"`) was deliberate over
`"packed"`: `"unpacked"` is the better dev-loop choice because the
linker only has to emit the index, not gather debuginfo into a
single `.dwp` file. `"packed"` is preferable for shipping artefacts
where a single debug bundle is desired, but this profile is
dev-only.
