# sbsh

[![CI](https://github.com/lispnik/sbsh/actions/workflows/ci.yml/badge.svg)](https://github.com/lispnik/sbsh/actions/workflows/ci.yml)
[![Release](https://img.shields.io/github/v/release/lispnik/sbsh?label=release)](https://github.com/lispnik/sbsh/releases/latest)

A Unix shell written in Common Lisp, built on **SBCL**. It leans on SBCL's
POSIX bindings and foreign-function interface rather than shelling out, giving
it real pipelines, real job control, and a from-scratch line editor with
history.

![sbsh demo](demo/sbsh-demo.gif)

```
$ make build
$ ./sbsh
example@host:~/Projects/common-lisp/sbsh$ echo hello | tr a-z A-Z
HELLO
```

## Features

- **Pipelines** — `a | b | c`, each stage its own process, wired together with
  `pipe(2)` / `dup2(2)`.
- **Redirections** — `<`, `>`, `>>`, `2>`, `2>>`, and fd duplication such as
  `2>&1`.
- **Operators** — `&&`, `||`, `;`, and background `&`.
- **Job control** — every pipeline runs in its own process group; the shell
  hands the terminal to the foreground group with `tcsetpgrp(3)` and reclaims
  it afterwards. `Ctrl-Z` suspends; `jobs`, `fg`, `bg`, and `kill %n` manage
  jobs; background completion is reported at the prompt.
- **History** — persisted to `~/.sbsh_history`, de-duplicated, navigable with
  the Up/Down arrows (or `C-p`/`C-n`) and searchable with `C-r`
  (reverse incremental search).
- **Line editor** — a raw-mode `readline` written from scratch: `C-a`/`C-e`
  (home/end), `C-b`/`C-f` (left/right), `C-k`/`C-u` (kill), `C-w` (kill word),
  `C-l` (clear), arrow keys, Delete, and **Tab filename completion**.
- **Expansion** — variables (`$VAR`, `${VAR}`, `$?`, `$$`), tilde (`~`), and
  globbing (`*`, `?`, `[abc]`, `[a-z]`, `[!…]`). Expansion happens at execution
  time, so `false; echo $?` and `export X=1; echo $X` behave correctly on one
  line.
- **Builtins** — `cd`, `pwd`, `exit`, `echo`, `export`, `unset`, `env`, `set`,
  `history`, `jobs`, `fg`, `bg`, `kill`, `type`, `help`, `alias`, `unalias`,
  `snapshot`, `true`, `false`, `:`.

## Common Lisp superpowers

Because the shell *is* a live SBCL image, it does things a POSIX shell cannot.

**A Lisp escape — any line starting with `(` is evaluated as Lisp:**

```
sbsh$ (+ 1 2 3)
6
sbsh$ echo "2^10 = $((expt 2 10))"     # arithmetic is just Lisp
2^10 = 1024
```

**Command substitution `$( … )` — shell *or* Lisp:**

```
sbsh$ echo "there are $(ls /usr/bin | wc -l | tr -d ' ') programs"
sbsh$ echo "sum: $((reduce (function +) (list 10 20 30)))"
```

**Lisp functions as pipeline stages** — a stage written `(...)` receives the
prior stage's output as the list `lines` and emits its result, so processes and
Lisp compose in one pipeline:

```
sbsh$ ls /etc | (remove-if-not (lambda (s) (search "conf" s)) lines) | sort | head
sbsh$ printf "b\na\nc\n" | (sort lines (function string<))
```

**Interactive error recovery via the condition system** — an unknown command
signals a correctable condition and offers "did you mean?" restarts:

```
sbsh$ gti status
sbsh: gti: command not found
Did you mean:
  [1] git
Run which? [1-1, Enter to cancel] 1
```

**Hot redefinition** — define or redefine a builtin at the prompt and use it
immediately; the running image changes underneath you:

```
sbsh$ (defcommand "hi" (args) (format t "hello ~A~%" (first args)))
sbsh$ hi world
hello world
```

**Homoiconic, queryable history** — every line is stored as structured data
(text, status, cwd, time, the commands it ran), queryable as Lisp:

```
sbsh$ (history-where (function failed-p))                       ; everything that failed
sbsh$ (history-where (lambda (e) (command-used-p "git" e)))     ; every git line
```

**`~/.sbshrc` is real Common Lisp** with a small DSL — see
[`demo/sample.sbshrc`](demo/sample.sbshrc):

```lisp
(defalias "ll" "ls -laFh")
(defcommand "mkcd" (args)                       ; a builtin in Lisp
  (ensure-directories-exist (concatenate 'string (first args) "/"))
  (sh (concatenate 'string "cd " (first args))))
(defcompletion "git" (word)                     ; context-aware Tab completion
  '("status" "commit" "checkout" "branch" "log"))
(defprompt () (format nil "~A sbsh> " (cwd)))   ; the prompt is a function
(on-cd (lambda (dir) (declare (ignore dir))))   ; hooks are closures
```

**Image snapshots** — `snapshot my-shell` dumps the live shell (with everything
you have defined this session) to a standalone executable via
`save-lisp-and-die`.

## How it uses SBCL

| Concern            | Mechanism                                                     |
|--------------------|---------------------------------------------------------------|
| Process creation   | `sb-posix:fork` + `execvp` (via `sb-alien`)                   |
| Pipelines          | `sb-posix:pipe`, `sb-posix:dup2`, `sb-posix:close`            |
| Process groups     | `sb-posix:setpgid`, `sb-posix:getpgrp`                        |
| Terminal control   | `tcsetpgrp`/`tcgetpgrp`/`isatty` (`sb-alien`)                 |
| Raw-mode editing   | `sb-posix` termios (`tcgetattr`/`tcsetattr`, `ICANON`/`ECHO`) |
| Window size        | `ioctl(TIOCGWINSZ)` via `sb-alien`                            |
| Reaping / status   | `sb-posix:waitpid` + `WIF*`/`WEXITSTATUS` macros              |
| Signals            | `sb-sys:enable-interrupt` (ignore in shell, default in child) |

## Dependencies

The shell itself depends on **nothing but SBCL** — just `sb-posix` and
`sb-alien`, which ship with the implementation.  The only external dependency
is for the test suite, managed with [ocicl](https://github.com/ocicl/ocicl):

- [`fiveam`](https://github.com/lispci/fiveam) — test framework (test system only)

Restore it with:

```
ocicl install
```

(Because the core has no external dependencies, `make build` works from a bare
SBCL checkout — no ocicl required unless you run the tests.)

## Install

Prebuilt binaries for Linux (amd64) and macOS (arm64) are attached to
each [release](https://github.com/lispnik/sbsh/releases):

```
tar xzf sbsh-<version>-<platform>-<arch>.tar.gz
./sbsh-<version>-<platform>-<arch>/sbsh
```

Or build from source (below).

## Building and running

```
make build     # produce ./sbsh (a standalone, compressed executable)
make test      # run the FiveAM suite
make run       # load and start the shell without building an image
./sbsh         # interactive
./sbsh -c 'ls -l | wc -l'   # one-shot
./sbsh script.sh            # run a script file
```

## Demo

```
make build
bash demo/demo.sh                 # non-interactive shell features
bash demo/lisp_demo.sh            # Common Lisp integration
python3 demo/interactive_demo.py  # line editor + job control, driven over a PTY
```

See [`demo/README.md`](demo/README.md) for what each covers.

## Layout

```
sbsh.asd            system + test-system definitions
src/
  package.lisp      packages (sbsh + sbsh-user) and global shell state
  conditions.lisp   condition types + Levenshtein for suggestions
  ffi.lisp          sb-alien: execvp, tcsetpgrp, tcgetpgrp, isatty
  terminal.lisp     raw/cooked termios modes, window size
  history.lisp      history storage/search + structured queryable records
  line-editor.lisp  raw-mode readline: keys, history, C-r, completion
  lexer.lisp        tokenizer: quoting, expansion, globbing, $(...) capture
  parser.lisp       clauses, pipelines (incl. Lisp stages), aliases
  jobs.lisp         job/process tracking, waitpid reaping
  builtins.lisp     built-in commands
  exec.lisp         fork/exec, process groups, fg/bg, Lisp eval, conditions
  config.lisp       user API + ~/.sbshrc DSL (defalias/defcommand/…)
  repl.lisp         the interactive loop and prompt
  main.lisp         entry point (-c / script / interactive)
tests/
  suite.lisp        unit tests for the pure layers
```

## Notes / limitations

- No here-documents `<<`.  Command substitution `$(…)` is supported (shell and
  Lisp), but not backticks.
- Aliases are word-level (no embedded pipes/operators).
- `echo` is POSIX-style (supports `-n`, not `-e`).
- Globbing follows the usual dotfile rule (a leading `.` must be matched
  explicitly).
- Tested on macOS (arm64) and Linux; `TIOCGWINSZ` is selected per platform.
