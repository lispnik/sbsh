# sbsh

A Unix shell written in Common Lisp, built on **SBCL**. It leans on SBCL's
POSIX bindings and foreign-function interface rather than shelling out, giving
it real pipelines, real job control, and a from-scratch line editor with
history.

```
$ make build
$ ./sbsh
mkennedy@host:~/Projects/common-lisp/sbsh$ echo hello | tr a-z A-Z
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
  `history`, `jobs`, `fg`, `bg`, `kill`, `type`, `help`, `true`, `false`, `:`.

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

Managed with [ocicl](https://github.com/ocicl/ocicl):

- [`alexandria`](https://alexandria.common-lisp.dev/) — utilities
- [`fiveam`](https://github.com/lispci/fiveam) — test framework (test system only)

Restore them with:

```
ocicl install
```

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
bash demo/demo.sh                 # non-interactive features
python3 demo/interactive_demo.py  # line editor + job control, driven over a PTY
```

See [`demo/README.md`](demo/README.md) for what each covers.

## Layout

```
sbsh.asd            system + test-system definitions
src/
  package.lisp      package + global shell state
  ffi.lisp          sb-alien: execvp, tcsetpgrp, tcgetpgrp, isatty
  terminal.lisp     raw/cooked termios modes, window size
  history.lisp      history storage, persistence, dedup, search
  line-editor.lisp  raw-mode readline: keys, history, C-r, completion
  lexer.lisp        tokenizer: quoting, expansion, globbing
  parser.lisp       clauses, pipelines, redirections
  jobs.lisp         job/process tracking, waitpid reaping
  builtins.lisp     built-in commands
  exec.lisp         fork/exec, process groups, fg/bg, and-or lists
  repl.lisp         the interactive loop and prompt
  main.lisp         entry point (-c / script / interactive)
tests/
  suite.lisp        unit tests for the pure layers
```

## Notes / limitations

- No command substitution `$(…)` or here-documents `<<`.
- `echo` is POSIX-style (supports `-n`, not `-e`).
- Globbing follows the usual dotfile rule (a leading `.` must be matched
  explicitly).
- Tested on macOS (arm64) and Linux; `TIOCGWINSZ` is selected per platform.
