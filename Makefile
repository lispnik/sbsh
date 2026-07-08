# Makefile for sbsh

LISP ?= sbcl
BIN  := sbsh

.PHONY: all build test run clean

all: build

## Build the standalone executable (./sbsh)
build:
	$(LISP) --non-interactive \
	  --eval '(asdf:make :sbsh)'

## Run the fiveam test suite
test:
	$(LISP) --non-interactive \
	  --eval '(asdf:load-system :sbsh/tests)' \
	  --eval '(uiop:quit (if (fiveam:run! (quote sbsh/tests:all-tests)) 0 1))'

## Load and run the shell interactively without building
run:
	$(LISP) --non-interactive \
	  --eval '(asdf:load-system :sbsh)' \
	  --eval '(sbsh:run-shell)'

clean:
	rm -f $(BIN)
	find . -name '*.fasl' -delete
