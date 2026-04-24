# Session summary 0077 — bd-511286 3 more empty-string rejections

## Goal

Close choices resolve/reissue + image generate empty-required bypasses.

## Bead(s)

- bd-511286 (self-filed)

## Before state

- Three surfaces leaked empty or surfaced raw reqwest errors.

## After state

- All three reject gold-standard at CLI boundary.

## Diff summary

- Commit: f9e790cc3913
- File: crates/caco-cli/src/lib.rs

## Operator-takeaway

caco image generate --prompt '' now tells you to describe the image
or use --preset instead of a cryptic URL-builder error.
