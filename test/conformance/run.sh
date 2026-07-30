#!/usr/bin/env bash
# Differential conformance harness for sbsh.
#
# Runs every case in posix.cases through sbsh and a reference POSIX shell
# (dash by default), comparing stdout + exit status.  dash is used as the
# oracle -- there is no separate "dash test suite", the reference shell IS
# the specification here.  Exits non-zero if any case diverges.
#
# Usage: run.sh [--verbose] [path/to/sbsh]
set -u

HERE="$(cd "$(dirname "$0")" && pwd)"
SBSH="${2:-${SBSH:-$HERE/../../sbsh}}"
REF="${REF:-$(command -v dash || echo /bin/dash)}"
CASES="$HERE/posix.cases"
VERBOSE=0
[ "${1:-}" = "--verbose" ] && VERBOSE=1

if [ ! -x "$SBSH" ]; then echo "sbsh not found/executable: $SBSH" >&2; exit 2; fi
if [ ! -x "$REF" ];  then echo "reference shell not found: $REF"   >&2; exit 2; fi

# Seed a scratch dir with fixtures the corpus expects.
WORK="$(mktemp -d)"; trap 'rm -rf "$WORK"' EXIT
( cd "$WORK" && touch a1 a2 a3 file.c foo.c existing three.txt )

# run SHELL CMD -> prints "<exit>\n<stdout>" (stderr discarded; wording differs)
run_one() {
  local shell="$1" cmd="$2" out ec
  out="$( cd "$WORK" && "$shell" -c "$cmd" 2>/dev/null )"; ec=$?
  printf '%s\n%s' "$ec" "$out"
}

pass=0; fail=0; failed_names=()
name=""; body=""; sbsh_cmd=""; dash_cmd=""

flush_case() {
  [ -z "$name" ] && return
  local sc dc a b
  sc="${sbsh_cmd:-$body}"; dc="${dash_cmd:-$body}"
  a="$(run_one "$SBSH" "$sc")"
  b="$(run_one "$REF"  "$dc")"
  if [ "$a" = "$b" ]; then
    pass=$((pass+1))
    [ "$VERBOSE" = 1 ] && printf '  PASS  %s\n' "$name"
  else
    fail=$((fail+1)); failed_names+=("$name")
    printf '  DIFF  %s\n' "$name"
    printf '        sbsh: %q\n' "$sc"
    printf '        got  (exit+stdout): %q\n' "$a"
    printf '        want (exit+stdout): %q\n' "$b"
  fi
  name=""; body=""; sbsh_cmd=""; dash_cmd=""
}

while IFS= read -r line || [ -n "$line" ]; do
  case "$line" in
    "===") flush_case ;;
    "#"*) : ;;                                  # comment
    "") [ -n "$body" ] && body+=$'\n' ;;        # blank line inside a body
    "name: "*) name="${line#name: }" ;;
    "sbsh: "*) sbsh_cmd="${line#sbsh: }" ;;
    "dash: "*) dash_cmd="${line#dash: }" ;;
    *) [ -n "$body" ] && body+=$'\n'; body+="$line" ;;
  esac
done < "$CASES"
flush_case

total=$((pass+fail))
echo "---------------------------------------------------------------"
printf 'conformance (sbsh vs %s): %d/%d passed' "$(basename "$REF")" "$pass" "$total"
[ "$fail" -gt 0 ] && printf '  (%d diverged: %s)' "$fail" "${failed_names[*]}"
echo
exit $(( fail > 0 ? 1 : 0 ))
