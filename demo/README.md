# sbsh demo

Two runnable demos that exercise the shell against the built `./sbsh` binary.
Build first with `make build` from the project root.

## Non-interactive features

```
bash demo/demo.sh
```

Walks through word splitting & quoting, pipelines, all redirection forms
(`> >> < 2> 2>&1`), `&& || ;`, variable / `$?` / `$$` / tilde expansion,
globbing, `VAR=value` assignments, built-ins, running a script file
(`demo/sample.sbsh`), and exit-status propagation.

## Interactive features (via a pseudo-terminal)

```
python3 demo/interactive_demo.py
```

Drives `sbsh` through a real PTY and prints an annotated transcript of:

- **A** — line editing (`Ctrl-A` to jump home, then insert)
- **B** — history recall with the Up arrow
- **C** — reverse incremental search (`Ctrl-R`)
- **D** — Tab filename completion
- **E** — background jobs (`&`, `jobs`, `kill %1`)
- **F** — `Ctrl-Z` suspend → `bg` resume → `jobs`

These need a TTY, which is why they run under `pty` rather than plain pipes.

## Files

| File                   | Purpose                                    |
|------------------------|--------------------------------------------|
| `demo.sh`              | batch driver for non-interactive features  |
| `sample.sbsh`          | a script file executed by `sbsh`           |
| `interactive_demo.py`  | PTY driver for the interactive line editor & job control |
