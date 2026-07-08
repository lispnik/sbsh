#!/usr/bin/env python3
"""interactive_demo.py --- drive sbsh through a real pseudo-terminal to show
the interactive features that only work on a TTY: the line editor, history
navigation, reverse search, tab completion, and job control.

Run from the project root:  python3 demo/interactive_demo.py
"""
import os, sys, pty, time, select, re

SBSH = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "sbsh")

CYAN, YELLOW, GREEN, DIM, RST = "\033[1;36m", "\033[1;33m", "\033[1;32m", "\033[2m", "\033[0m"

def title(t): print(f"\n{CYAN}══════ {t} ══════{RST}")
def note(t):  print(f"{DIM}   … {t}{RST}")

def session(steps, settle=0.35, boot=0.7):
    """Spawn sbsh under a PTY, send scripted keystrokes, echo everything back."""
    pid, fd = pty.fork()
    if pid == 0:
        env = dict(os.environ); env["TERM"] = "xterm"
        os.execve(SBSH, [SBSH], env)
    captured = []
    def drain(t):
        end = time.time() + t
        while time.time() < end:
            r, _, _ = select.select([fd], [], [], 0.05)
            if r:
                try: data = os.read(fd, 4096)
                except OSError: return
                if not data: return
                captured.append(data.decode("utf-8", "replace"))
                end = time.time() + t
    drain(boot)
    for keys, desc, wait in steps:
        if desc: note(desc)
        os.write(fd, keys.encode())
        drain(wait)
    try: os.close(fd); os.waitpid(pid, 0)
    except OSError: pass
    # Clean the raw stream into readable lines.
    text = "".join(captured)
    text = re.sub(r"\x1b\[[0-9;]*[A-Za-z]", "", text)   # strip CSI
    text = re.sub(r"\x1b.", "", text)                    # stray escapes
    text = text.replace("\r", "")
    # Collapse the per-keystroke prompt redraws into just the final line each.
    out = []
    for line in text.split("\n"):
        # keep the last prompt-render on a line (drop intermediate redraws)
        if "$ " in line:
            line = line[line.rfind("$ ") - 40 if line.rfind("$ ") > 40 else 0:]
            line = "…$ " + line.split("$ ")[-1]
        if line.strip():
            out.append(line)
    print(f"{YELLOW}--- transcript ---{RST}")
    print("\n".join(out))

title("A. Line editing: type 'world', jump home (C-a), prepend 'echo hello '")
session([
    ("world",        "type 'world'", 0.25),
    ("\x01",         "Ctrl-A  → move to start of line", 0.2),
    ("echo hello ",  "type 'echo hello ' at the start", 0.25),
    ("\n",           "Enter   → runs 'echo hello world'", 0.4),
    ("exit\n", "", 0.3),
])

title("B. History: run two commands, then Up-arrow twice to recall the first")
session([
    ("echo apple\n",  "run 'echo apple'", 0.4),
    ("echo banana\n", "run 'echo banana'", 0.4),
    ("\x1b[A",        "Up-arrow → recalls 'echo banana'", 0.3),
    ("\x1b[A",        "Up-arrow → recalls 'echo apple'", 0.3),
    ("\n",            "Enter    → re-runs 'echo apple'", 0.4),
    ("exit\n", "", 0.3),
])

title("C. Reverse incremental search (Ctrl-R)")
session([
    ("echo find_me_123\n", "run 'echo find_me_123'", 0.4),
    ("echo distraction\n", "run 'echo distraction'", 0.4),
    ("\x12",               "Ctrl-R → enter reverse search", 0.3),
    ("find",               "type 'find' → matches the earlier command", 0.4),
    ("\n",                 "Enter  → runs the matched command", 0.4),
    ("exit\n", "", 0.3),
])

title("D. Tab completion of a filename")
session([
    ("ls -d /etc/pass",  "type 'ls -d /etc/pass'", 0.3),
    ("\t",               "Tab → completes to /etc/passwd", 0.4),
    ("\n",               "Enter", 0.4),
    ("exit\n", "", 0.3),
])

title("E. Job control: background a job, list it, then a pipeline")
session([
    ("sleep 30 &\n", "background 'sleep 30 &'  → prints [1] <pid>", 0.5),
    ("jobs\n",       "jobs → shows it Running", 0.5),
    ("kill %1\n",    "kill %1 → sends SIGTERM to the job", 0.5),
    ("echo done\n",  "", 0.4),
    ("exit\n", "", 0.4),
])

title("F. Job control: Ctrl-Z suspends a foreground job, bg resumes it")
session([
    ("sleep 30\n", "start foreground 'sleep 30'", 0.5),
    ("\x1a",       "Ctrl-Z → suspends it  → [1]+ Stopped", 0.6),
    ("jobs\n",     "jobs → shows it Stopped", 0.5),
    ("bg\n",       "bg   → resumes in background → Running", 0.5),
    ("jobs\n",     "jobs → now Running", 0.5),
    ("kill %1\n",  "clean up", 0.5),
    ("exit\n", "", 0.4),
])

print(f"\n{GREEN}Interactive demo complete.{RST}")
