# Conformance: Oil/OSH spec tests

This directory grades `sbsh` against the **Oil/OSH spec-test corpus**, a large,
cross-shell suite where each case records the expected stdout/status for several
reference shells (bash, dash, mksh, zsh).

- `oils/` — a git submodule of [oils-for-unix/oils](https://github.com/oils-for-unix/oils)
  (Apache-2.0), sparse-checked-out to `spec/` only. It provides the `.test.sh`
  corpus and its Python helper scripts (`spec/bin/argv.py`, …).
- `run.py` — a **self-contained** grader (it does *not* use Oil's build system).
  It parses the `.test.sh` format directly and grades sbsh **as a POSIX shell**:
  it compares sbsh's stdout/status to the **dash** expectation for each case
  (the `## OK dash` / `## BUG dash` override when present, else the default).
  Cases dash itself does not implement (`## N-I dash`) are skipped, not failed.

## Running

```
git submodule update --init --depth 1 test/spec/oils   # once
make build
make spec                    # or: python3 test/spec/run.py [--verbose] [file...]
```

The score is **informational** — many cases exercise bash-only features sbsh
deliberately does not target, so `run.py` always exits 0. Its value is as a
**regression signal**: the per-file and total percentages should not drop as the
code changes, and should climb as conformance work lands. Low categories map to
known gaps (e.g. `getopts`, `read`'s bash options, `echo -e`).

For the hard-gated, dash-differential check on a hand-curated POSIX corpus, see
`../conformance/` (run by `make conformance`).

## Notes on interpreting the score

sbsh is graded against dash, so a bash-specific case that dash also fails is
either an `## N-I dash` (skipped) or simply not counted in sbsh's favor. The
number is a conservative floor on POSIX conformance, not a bug count.
