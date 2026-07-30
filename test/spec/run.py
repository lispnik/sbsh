#!/usr/bin/env python3
"""Grade sbsh against the vendored Oil/OSH spec corpus.

The Oil `.test.sh` files encode, per case, the expected stdout/status for several
reference shells (bash/dash/mksh/zsh) via `## OK <shell>` / `## N-I <shell>`
overrides.  sbsh targets POSIX sh, so we grade it against the **dash** view:
the dash override when present, otherwise the default expectation.  Cases dash
does not implement (`## N-I dash`) are skipped, not failed.

This is a *self-contained* runner -- it parses the format directly and does not
depend on Oil's build system.  It is informational: it prints a score and never
fails the build (many cases exercise bash-only features sbsh doesn't target).

Usage: run.py [--verbose] [--shell PATH] [spec-file ...]
"""
import os, sys, re, json, subprocess, tempfile, shutil

HERE = os.path.dirname(os.path.abspath(__file__))
SPEC = os.path.join(HERE, "oils", "spec")
TARGET = "dash"          # grade sbsh as a POSIX (dash-like) shell

# A curated, POSIX-relevant subset (bash arrays/assoc/extglob files excluded).
CURATED = [
    "arith", "word-split", "quote", "loop", "case_", "if_", "var-sub",
    "var-op-strip", "var-op-test", "assign", "and-or", "pipeline", "redirect",
    "glob", "sh-func", "command-sub", "comments", "dbracket-i18n",
    "builtin-eval-source", "builtin-bracket", "builtin-cd", "builtin-echo",
    "builtin-read", "builtin-special", "builtin-trap", "builtin-getopts",
    "sh-options", "exit-status", "special-vars",
]

def load_cases(path):
    """Yield (name, body, expected) dicts for each #### case in PATH.
    EXPECTED maps a key like 'stdout'/'status'/'stdout-json', possibly scoped by
    shell ('OK dash', 'N-I dash', 'BUG dash'), to its value."""
    cases, cur = [], None
    lines = open(path, encoding="utf-8", errors="replace").read().splitlines()
    i = 0
    while i < len(lines):
        ln = lines[i]
        if ln.startswith("#### "):
            if cur: cases.append(cur)
            cur = {"name": ln[5:].strip(), "body": [], "ann": {}}
        elif cur is not None and (ln.startswith("## ")):
            m = re.match(r"## (?:(OK|N-I|BUG) (\w+) )?"
                         r"(STDOUT|stdout|stdout-json|status|stderr-json):(.*)$", ln)
            if m:
                kind, shell, key, val = m.groups()
                scope = f"{kind} {shell}" if kind else ""
                if key == "STDOUT":                       # multi-line block to ## END
                    block = []
                    i += 1
                    while i < len(lines) and lines[i].strip() != "## END":
                        block.append(lines[i]); i += 1
                    cur["ann"][(scope, "stdout")] = "\n".join(block) + ("\n" if block else "")
                elif key == "stdout":
                    # single-line form: expected output is the value + one newline
                    cur["ann"][(scope, "stdout")] = val.strip() + "\n"
                else:
                    cur["ann"][(scope, key)] = val.strip()
            # non-matching ## lines (e.g. "## our_shell") are ignored
        elif cur is not None:
            cur["body"].append(ln)
        i += 1
    if cur: cases.append(cur)
    return cases

def expectation(ann, target):
    """Resolve (status, stdout, kind) for TARGET shell. kind: 'run'/'ni'/'none'."""
    def pick(key):
        # dash-specific override wins over the default
        for scope in (f"OK {target}", f"BUG {target}", ""):
            if (scope, key) in ann: return ann[(scope, key)]
        return None
    if any(s == f"N-I {target}" for (s, _k) in ann):
        return (None, None, "ni")
    status = pick("status")
    sj = pick("stdout-json")
    if sj is not None:
        try: out = json.loads(sj)
        except Exception: out = None
    else:
        out = pick("stdout")          # already includes its trailing newline(s)
    if out is None and status is None:
        return (None, None, "none")
    return (int(status) if status is not None else 0, out, "run")

def run_sbsh(shell, body, env):
    # Each case runs in its own scratch cwd: many cases create files, and cases
    # run in parallel, so a shared directory would both pollute the repo and race.
    d = tempfile.mkdtemp()
    try:
        p = subprocess.run([shell, "-c", body], capture_output=True, text=True,
                           timeout=5, env=env, stdin=subprocess.DEVNULL, cwd=d)
        return p.returncode, p.stdout
    except subprocess.TimeoutExpired:
        return 124, ""
    except Exception:
        return 125, ""
    finally:
        shutil.rmtree(d, ignore_errors=True)

def main():
    args = sys.argv[1:]
    verbose = "--verbose" in args
    args = [a for a in args if a != "--verbose"]
    shell = os.environ.get("SBSH", os.path.join(HERE, "..", "..", "sbsh"))
    if args and args[0] == "--shell":
        shell = args[1]; args = args[2:]
    shell = os.path.abspath(shell)
    if not os.path.exists(shell):
        print(f"sbsh not found: {shell}", file=sys.stderr); return 2
    if not os.path.isdir(SPEC):
        print(f"oils spec corpus not found at {SPEC}\n"
              f"run: git submodule update --init --depth 1 test/spec/oils", file=sys.stderr)
        return 2

    files = args or [os.path.join(SPEC, f + ".test.sh") for f in CURATED]
    files = [f for f in files if os.path.exists(f)]

    # PATH: a python2->python3 shim (Oil helpers have #!/usr/bin/env python2) + spec/bin
    shim = tempfile.mkdtemp()
    with open(os.path.join(shim, "python2"), "w") as fh:
        fh.write('#!/bin/sh\nexec python3 "$@"\n')
    os.chmod(os.path.join(shim, "python2"), 0o755)
    env = dict(os.environ)
    env["PATH"] = f"{shim}:{os.path.join(SPEC, 'bin')}:{env['PATH']}"

    from concurrent.futures import ThreadPoolExecutor
    try:
        names = [os.path.basename(f).replace(".test.sh", "") for f in files]
        stats = {i: [0, 0, 0, 0, []] for i in range(len(files))}   # pass fail ni skip fails
        work = []
        for i, f in enumerate(files):
            for c in load_cases(f):
                st, out, kind = expectation(c["ann"], TARGET)
                if kind == "ni":   stats[i][2] += 1; continue
                if kind == "none": stats[i][3] += 1; continue
                work.append((i, c["name"], "\n".join(c["body"]), st, out))

        def run_case(item):
            i, nm, body, st, out = item
            rc, o = run_sbsh(shell, body, env)
            return i, nm, (o == out) and (st is None or rc == st)

        with ThreadPoolExecutor(max_workers=min(16, (os.cpu_count() or 4) * 2)) as ex:
            for i, nm, ok in ex.map(run_case, work):
                if ok: stats[i][0] += 1
                else:  stats[i][1] += 1; stats[i][4].append(nm)

        tp = tf = tn = 0
        for i, name in enumerate(names):
            p, fl, ni, _sk, fails = stats[i]
            tp += p; tf += fl; tn += ni
            total = p + fl
            pct = (100 * p // total) if total else 0
            print(f"  {name:<24} {p:3}/{total:<3} ({pct:3}%)" + (f"   n-i:{ni}" if ni else ""))
            if verbose:
                for nm in fails: print(f"        FAIL  {nm}")
        gt = tp + tf
        print("-" * 60)
        print(f"oils spec (sbsh graded as {TARGET}): {tp}/{gt} "
              f"({100*tp//gt if gt else 0}%)   [{tn} dash-N-I skipped]")
    finally:
        shutil.rmtree(shim, ignore_errors=True)
    return 0   # informational: never fail the build

if __name__ == "__main__":
    sys.exit(main())
