# Session summary — Aurora token canonicalization in the hub (md2-1)

## Goal
As UX-hub + canonical-token-spec owner, canonicalize the multiple "aurora" efforts (premium AURORA, po4-0 Aurora-Glass, msd-1 figma renders) into one coherent hub framing so the direction is clear for Harry and nothing fragments.

## What landed
- `companion/android/docs/ux-revamp-2026.md` §17 "Token canonicalization": the reconciled framing agreed across the android crew (md2-1/md2-0/msd-1/po4-0) — the two token directions are DISTINCT (different palettes/postures) but RELATED as a phasing spectrum, not rivals: Aurora Glass conservative = the low-risk Phase-0 on-ramp; AURORA premium = the aspirational destination (landed + tappable). AuroraTokens.kt is the canonical token source of truth (only direction with runnable in-app code). Harry picks the intensity + phasing.

## Reference layer canonicalized
- Premium visual target: msd-1's mockups/stunning/ (3 figma-fidelity premium-dark renders).
- Conservative variant: po4-0's mockups/aurora-glass-tokens.md (Phase-0 on-ramp).
- Superseded: msd-1's stunning/README.md pre-convergence token sketch — superseded by AuroraTokens.kt.

## Coordination
- Synced the framing with msd-1 (UI lead) + md2-0: started at "two distinct bets" vs msd-1's "one family two intensities", converged on "distinct directions related as Phase-0 on-ramp -> premium destination" so the hub reads consistently (md2-0 flagged the framing mismatch; resolved).

## Operator-takeaway
- Single hub now cleanly canonicalizes all aurora efforts: landed AuroraTokens.kt = canon, msd-1 renders = premium visual reference, po4-0 tokens = conservative Phase-0, with the choice (intensity + phasing) framed for Harry. The tappable AURORA preview is his decision centerpiece. Docs-only — echo gate.
