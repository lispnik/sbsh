#!/usr/bin/env bash
# lisp_demo.sh --- demonstrate sbsh's Common Lisp integration.
# Usage: bash demo/lisp_demo.sh   (run from the project root)

set -u
SBSH="$(cd "$(dirname "$0")/.." && pwd)/sbsh"
[ -x "$SBSH" ] || { echo "run 'make build' first" >&2; exit 1; }

hr()  { printf '\n\033[1;36m── %s\033[0m\n' "$1"; }
run() { printf '\033[1;33msbsh$ %s\033[0m\n' "$1"; "$SBSH" -c "$1"; }

hr "1. A Lisp escape: any line starting with ( is evaluated as Lisp"
run '(+ 1 2 3 4 5)'
run '(string-upcase "hello from lisp")'

hr "2. Arithmetic is just Lisp: \$(( ... ))"
run 'echo "2^10 = $((expt 2 10))"'
run 'echo "$((/ (* 22 7) 7.0))"'

hr "3. Command substitution \$( ... ) — shell OR Lisp"
run 'echo "kernel: $(uname -s), files here: $(ls | wc -l | tr -d " ")"'
run 'echo "lisp says: $((reduce (function +) (list 10 20 30)))"'

hr "4. Lisp functions as pipeline stages (\`lines\` = input lines)"
run 'printf "banana\napple\ncherry\n" | (sort lines (function string<))'
run 'printf "a\nb\nc\n" | (mapcar (function string-upcase) lines)'
run 'ls /usr/bin | (length lines) '

hr "5. Mixing Unix processes and Lisp in one pipeline"
run 'ls /etc | (remove-if-not (lambda (s) (search "conf" s)) lines) | sort | head -5'

hr "6. The full language is available (here: define and call a function)"
run '(progn (defun sq (x) (* x x)) (mapcar (function sq) (list 1 2 3 4 5)))'

printf '\n\033[1;32mLisp-integration demo complete.\033[0m\n'
printf '\033[2m(Interactive-only features — the command-not-found menu, hot\n'
printf 'redefinition, and per-command completion — are in interactive_demo.py)\033[0m\n'
