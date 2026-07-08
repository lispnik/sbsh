#!/usr/bin/env python3
"""Record demo/sbsh-demo.gif: drive ./sbsh through a PTY with a redacted
prompt (example@host), write an asciinema v2 cast, and render it with `agg`.

Requires: a built ./sbsh (run `make build`) and `agg` on PATH.
Usage: python3 demo/record_gif.py
"""
import os, pty, time, select, json, subprocess, tempfile, shutil, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SBSH = os.path.join(ROOT, "sbsh")
GIF = os.path.join(ROOT, "demo", "sbsh-demo.gif")
WIDTH, HEIGHT = 84, 22

if not os.path.exists(SBSH):
    sys.exit("build first: make build")
if not shutil.which("agg"):
    sys.exit("need `agg` (asciinema gif generator) on PATH")

work = tempfile.mkdtemp(prefix="sbsh-demo-")
home = os.path.join(work, "home"); demo = os.path.join(work, "sbsh-demo")
os.makedirs(home); os.makedirs(demo)
for n in ("alpha.txt", "banana.md", "cherry.log", "delta.txt"):
    open(os.path.join(demo, n), "w").close()
# Redacting prompt: username -> example, host -> host, cwd -> basename.
with open(os.path.join(home, ".sbshrc"), "w") as f:
    f.write(r'''(defprompt ()
  (format nil "~C[1;32mexample@host~C[0m:~C[1;34m~A~C[0m$ "
          #\Escape #\Escape #\Escape
          (car (last (pathname-directory (concatenate 'string (cwd) "/"))))
          #\Escape))
''')

pid, fd = pty.fork()
if pid == 0:
    os.chdir(demo)
    os.execve(SBSH, [SBSH], {
        "TERM": "xterm-256color", "USER": "example", "HOME": home,
        "PATH": os.environ.get("PATH", "/usr/bin:/bin"),
        "LANG": "en_US.UTF-8", "PWD": demo})

import fcntl, termios, struct
fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", HEIGHT, WIDTH, 0, 0))

events, t0 = [], time.time()
def pump(dur):
    end = time.time() + dur
    while time.time() < end:
        r, _, _ = select.select([fd], [], [], 0.02)
        if r:
            try: data = os.read(fd, 4096)
            except OSError: return
            if not data: return
            events.append([round(time.time() - t0, 4), "o", data.decode("utf-8", "replace")])
            end = time.time() + dur
def send(s): os.write(fd, s.encode())
def typ(s, cps=0.05):
    for ch in s: send(ch); pump(cps)
def cmd(line, think=0.35, after=0.7):
    pump(think); typ(line); pump(0.25); send("\r"); pump(after)

pump(1.0)
cmd("echo hello world | tr a-z A-Z", after=0.8)
cmd("(+ 40 2)", after=0.8)
cmd("echo \"there are $(ls | wc -l | tr -d ' ') files\"", after=0.9)
cmd("ls | (sort lines #'string>)", after=1.0)
pump(0.5); send("\x1b[A"); pump(0.7); send("\x1b[A"); pump(0.7); send("\x03"); pump(0.6)
cmd("ecko hi there", after=0.8)
typ("1"); pump(0.2); send("\r"); pump(1.0)
cmd("sleep 3 &", after=0.7)
cmd("jobs", after=1.0)
pump(0.3); typ("exit"); pump(0.2); send("\r"); pump(0.8)

try: os.close(fd); os.waitpid(pid, 0)
except OSError: pass

cast = os.path.join(work, "sbsh.cast")
with open(cast, "w") as f:
    f.write(json.dumps({"version": 2, "width": WIDTH, "height": HEIGHT,
                        "timestamp": int(t0),
                        "env": {"TERM": "xterm-256color", "SHELL": "sbsh"}}) + "\n")
    for e in events: f.write(json.dumps(e) + "\n")

subprocess.run(["agg", "--font-size", "20", "--idle-time-limit", "2",
                "--theme", "monokai", cast, GIF], check=True)
shutil.rmtree(work, ignore_errors=True)
print("wrote", GIF)
