#!/usr/bin/env bash
# demo.sh --- drive sbsh through its non-interactive features.
# Usage: bash demo/demo.sh   (run from the project root)

set -u
SBSH="$(cd "$(dirname "$0")/.." && pwd)/sbsh"

if [ ! -x "$SBSH" ]; then
    echo "sbsh binary not found; run 'make build' first." >&2
    exit 1
fi

hr()  { printf '\n\033[1;36m── %s\033[0m\n' "$1"; }
run() { printf '\033[1;33msbsh$ %s\033[0m\n' "$1"; "$SBSH" -c "$1"; }

hr "1. Simple commands and word splitting"
run 'echo hello   world'
run 'echo "quotes preserve   spacing" and '\''single quotes are literal $HOME'\'

hr "2. Pipelines (each stage is its own process)"
run 'printf "3\n1\n4\n1\n5\n9\n2\n6\n" | sort -n | uniq | tr "\n" " "'
run 'ls /usr/bin | wc -l | tr -d " " | xargs -I{} echo "{} programs in /usr/bin"'

hr "3. Redirections: > >> < 2> 2>&1"
run 'echo first  > /tmp/sbsh_demo.txt; echo second >> /tmp/sbsh_demo.txt; cat /tmp/sbsh_demo.txt'
run 'wc -l < /tmp/sbsh_demo.txt'
run 'ls /does/not/exist 2>/tmp/sbsh_err.txt; echo "captured stderr:"; cat /tmp/sbsh_err.txt'
run 'ls /nope 2>&1 | grep -c .'

hr "4. Logical operators && || and sequencing ;"
run 'true  && echo "&& runs on success"'
run 'false || echo "|| runs on failure"'
run 'true && echo A && false && echo B; echo "B was skipped, status=$?"'

hr "5. Expansion: variables, \$?, \$\$, tilde"
run 'X=42; echo "X=$X  status=$?  home=~"'
run 'export GREET=hi; echo "${GREET}, world"'
run 'false; echo "last status was $?"'

hr "6. Globbing (* ? [..])"
run 'cd /tmp && ls sbsh_demo*.txt 2>/dev/null; echo "---"; echo /tmp/sbsh_demo?.txt'

hr "7. Variable assignments"
run 'A=1 B=2 C=3; echo "$A$B$C"'

hr "8. Built-ins"
run 'type cd; type ls; type nope'
run 'cd /tmp && pwd'
run 'echo -n "no newline here"; echo " <- joined"'

hr "9. Running a script file"
"$SBSH" "$(dirname "$0")/sample.sbsh"

hr "10. Exit status propagation"
"$SBSH" -c 'exit 7'; echo "sbsh -c 'exit 7' returned: $?"

printf '\n\033[1;32mNon-interactive demo complete.\033[0m\n'
