# Makefile for sbsh

LISP ?= sbcl
BIN  := sbsh

# Load ASDF and register this repo's system definition explicitly, so every
# target works from a fresh clone with no source-registry / ocicl setup.
LOAD_ASD = --eval '(require :asdf)' \
           --eval '(asdf:load-asd (truename "sbsh.asd"))'

.PHONY: all build test run clean conformance spec

all: build

## Build the standalone executable ./sbsh via ASDF's program-op
## (declared as the :build-operation in sbsh.asd; asdf:make dispatches to it and
## the sb-core-compression method dumps a compressed, executable image).
build:
	$(LISP) --non-interactive $(LOAD_ASD) \
	  --eval '(asdf:make :sbsh)'

## Run the fiveam test suite (needs fiveam -- run `ocicl install` first)
test:
	$(LISP) --non-interactive $(LOAD_ASD) \
	  --eval '(asdf:load-system :sbsh/tests)' \
	  --eval '(uiop:quit (if (fiveam:run! (quote sbsh/tests:all-tests)) 0 1))'

## Load and run the shell interactively without building
run:
	$(LISP) --non-interactive $(LOAD_ASD) \
	  --eval '(asdf:load-system :sbsh)' \
	  --eval '(sbsh:run-shell)'

## Differential conformance check: diff sbsh against dash on a POSIX corpus.
## Fails if any case diverges.  Requires ./sbsh and dash.
conformance: build
	bash test/conformance/run.sh

## Oil/OSH spec-test score: grade sbsh (as a POSIX shell) against the vendored
## oils spec corpus.  Informational -- prints a score, never fails the build.
## Requires the git submodule under test/spec/oils (git submodule update --init).
spec: build
	python3 test/spec/run.py

clean:
	rm -f $(BIN)
	find . -name '*.fasl' -delete
