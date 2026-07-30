# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`sbsh` is a Unix shell written in Common Lisp on **SBCL**. It implements pipelines,
job control, and a from-scratch line editor by calling POSIX directly through
`sb-posix` and `sb-alien` — it does *not* shell out to another shell. The shell
core depends on nothing but SBCL; the only external dependency is `fiveam`, used
by the test system only. Dependencies are vendored under `ocicl/` and managed
with [ocicl](https://github.com/ocicl/ocicl).

## Commands

```
make build     # produce ./sbsh (standalone, compressed executable via asdf:make)
make test      # run the FiveAM suite (exits non-zero on failure)
make run       # load and start the shell without building an image
make clean      # remove ./sbsh and *.fasl

ocicl install  # restore vendored deps (only needed for the test suite)
```

Run the built shell:

```
./sbsh                       # interactive
./sbsh -c 'ls -l | wc -l'    # one-shot
./sbsh script.sh             # run a script file
```

There is no way to run a single FiveAM test from the Makefile. To run one test
by name during development, load the test system and call it directly:

```
sbcl --non-interactive \
  --eval '(asdf:load-system :sbsh/tests)' \
  --eval '(fiveam:run! (quote sbsh/tests::TEST-NAME))'
```

Test names live in the `sbsh/tests` package (they reach into the shell with
`sbsh::`), so a single test is `sbsh/tests::fnmatch-star`, not `sbsh::...`.
There is only one suite, `sbsh/tests:all-tests`; `make test` runs it.

CI (`.github/workflows/ci.yml`) runs `make test` then `make build` on
ubuntu-latest and macos-latest, and smoke-tests the binary with `--version`
and a couple of `-c` pipelines.

## Architecture

The `:serial t` component order in `sbsh.asd` is also the dependency order —
each file may use anything defined above it. The pipeline of a command line is:

**read → strip comments → split into clause segments → (per segment, lazily) tokenize → parse → realize → execute.**

The key design decision is that **expansion happens at execution time, not parse
time.** `parser.lisp` splits a line into clause *segments* on the top-level
control operators (`;`, `&&`, `||`, `&`) *without* expanding anything; each
segment is then tokenized, parsed, and realized only when it is about to run.
This is why `false; echo $?`, `cd` side effects, and mid-line env changes behave
correctly. `COMMAND` structs carry raw `words`/`redir-specs`; `argv`/`redirs`
are filled in by `REALIZE-COMMAND` at execution time.

Layers, in the exact `:serial t` load order from `sbsh.asd` (each file may use
anything above it — so, e.g., the line editor loads *before* the lexer and
cannot depend on it, while `grammar.lisp` loads late and can call into `exec`):

- `package.lisp` — the `sbsh` package plus a separate **`sbsh-user`** package.
  All interactive `(...)` Lisp escapes and `~/.sbshrc` evaluate in `sbsh-user`,
  which sees CL + a curated API but *not* sbsh internals. This file also holds
  **all global shell state** as `defvar`s (`*last-status*`, `*aliases*`,
  `*functions*`, `*positional*`, the `set` options
  `*errexit*`/`*nounset*`/`*pipefail*`/`*noclobber*`/`*noglob*`/`*xtrace*`,
  `*pipestatus*`, `*last-bg-pid*` (for `$!`), `*clause-negated*` (errexit `!`
  exemption), `*readonly-vars*`, `*traps*`, heredoc/assignment restore stacks,
  etc.). When adding shell behavior, prefer extending this state model over
  threading new arguments.
- `util.lisp` — two dependency-free helpers (`starts-with-subseq`,
  `read-file-into-string`) kept in-tree to avoid a runtime utility dependency.
- `conditions.lisp` — `shell-error`/`command-not-found` conditions plus
  `levenshtein` (powers "did you mean?" suggestions on a missing command).
- `ffi.lisp` — `sb-alien` bindings: `execvp`, `tcsetpgrp`, `tcgetpgrp`, `isatty`.
- `terminal.lisp` — raw/cooked termios modes, `TIOCGWINSZ` window size.
- `history.lisp` — command history storage, dedup, and `~/.sbsh_history`
  persistence (`*history-persist*` is bound off by tests).
- `line-editor.lisp` — raw-mode readline (keys, history nav, `C-r`, Tab
  completion). Tab completion is **context-aware** (`complete-token`): command
  names in command position (builtins + functions + aliases + `$PATH` +
  reserved words), directories-only after `cd`, `$VAR` names after a `$`, `defcompletion` hooks, else
  filenames.
- `lexer.lisp` — tokenizer: quoting, all variable/parameter expansion, `$'...'`
  ANSI-C quoting, POSIX arithmetic `$((...))` (`eval-arithmetic`, falling back
  to Lisp for non-POSIX forms), command substitution `$(...)`, globbing/`fnmatch`
  (memoized — no ReDoS), word-splitting on `$IFS`.
- `parser.lisp` — clause segmentation, pipelines (including Lisp `(...)` stages
  and `subshell { }`), aliases, `strip-comment`, and `validate-clauses`
  (syntax errors for stray/empty control operators).
- `jobs.lisp` — job/process tracking, `waitpid` reaping.
- `builtins.lisp` — built-in commands (`cd`, `export`, `readonly`, `read`,
  `test`/`[`, `set`, `eval`, `.`/`source`, `exec`, `trap`, `getopts`, `jobs`/`fg`/`bg`,
  etc.).
- `exec.lisp` — the core: `fork`/`exec`, process groups, `tcsetpgrp` handoff,
  fg/bg, Lisp-stage evaluation, `subshell`/EXIT-trap running, and the
  interactive condition system.
- `grammar.lisp` — compound commands (`if`/`while`/`until`/`for`/`case`,
  `break`/`continue`). A compound is stored by the parser as *raw source text*
  and re-parsed/interpreted here via `RUN-COMMAND-LINE` so expansions re-run
  each iteration.
- `config.lisp` — the user-facing API and `~/.sbshrc` DSL (`defalias`,
  `defcommand`, `defprompt`, `defcompletion`, `on-cd`, `sh`, ...).
- `repl.lisp` / `main.lisp` — the interactive loop and the entry point.

### Things to know

- **Job control model:** every pipeline becomes its own process group; the shell
  hands the terminal to the foreground group with `tcsetpgrp` and reclaims it
  afterward. `init-job-control` in `exec.lisp` sets this up. When touching
  process/signal code, respect the ignore-in-shell / default-in-child split of
  `*jobctl-signals*`.
- **Lisp pipeline stages vs subshells:** a stage written `(...)` is a Lisp
  filter that receives the prior stage's output as `sbsh-user::lines` (a list)
  and `sbsh-user::input` (the whole string), interned in `sbsh-user` so user
  forms like `ls | (sort lines)` resolve. Because bare `(...)` is Lisp, a real
  POSIX subshell is spelled **`subshell { ... }`** (a `:subshell` special that
  `%launch-pipeline` routes through the fork path so its `cd`/env/`set` stay
  isolated). Don't try to make bare `(...)` a subshell — it's a deliberate
  design choice, and `$((...))` arithmetic falls back to Lisp for the same
  reason.
- **Expansion errors vs other shell errors:** `conditions.lisp` splits
  `expansion-error` (e.g. `${v:?}`, `set -u` on an unset var — exits a
  non-interactive shell, status 2) from plain `shell-error` (e.g. a noclobber
  redirection failure — fails the command, status 1, keeps going).
  `run-clause` in `exec.lisp` handles both, plus a `storage-condition` guard so
  a pathological input can't take down an interactive shell.
- **Two "parsers":** `parser.lisp` handles the token/pipeline grammar;
  `grammar.lisp` handles control-flow keywords by text-scanning. Compound-command
  work usually belongs in `grammar.lisp`, not `parser.lisp`.
- **Tests cover the pure layers** (fnmatch, tokenizer, parser, expansion) and
  reach into internals with `sbsh::` — e.g. `(sbsh::parse-line ...)`,
  `(sbsh::tokenize ...)`, `(sbsh::fnmatch ...)`. Prefer adding conformance tests
  at this level rather than driving the built binary. For behavior that needs
  the interpreter (loops, errexit, eval), the suite has a `run-line` helper that
  runs a line in-process with shell globals freshly bound and returns
  `(values stdout status)`.
- **POSIX conformance is an active, ongoing effort** — commits are numbered
  "Round N conformance" (currently through Round 10, which was driven by a
  differential audit against `dash`/`bash` and fixed 52 findings). New
  shell-behavior changes should come with a matching test in `tests/suite.lisp`
  and ideally a note in the README Features/Notes sections. When a fix conflicts
  with an sbsh design choice, prefer documenting the deviation over breaking the
  design (see the subshell and prefix-assignment notes below).

### Known limitations (from README — don't treat as bugs)

- Bare `( ... )` is a **Lisp filter stage**, not a POSIX subshell; use
  `subshell { ... }` for a forked, isolated subshell.
- Prefix assignments are not applied strictly left-to-right, so a later one
  can't see an earlier one on the same command (`a=1 b=$a cmd` sees `b` empty).
  This follows from expansion-at-tokenize-time; independent prefix assignments
  (`LANG=C LC_ALL=C sort`) work.
- Aliases are word-level (no embedded pipes/operators).
- `echo` is POSIX-style: `-n` yes, `-e` no.
