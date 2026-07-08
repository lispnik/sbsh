;;;; package.lisp --- Package definition for sbsh.

(defpackage #:sbsh
  (:use #:cl)
  (:import-from #:alexandria
                #:when-let #:if-let #:ensure-list #:starts-with-subseq
                #:read-file-into-string #:deletef #:removef #:last-elt)
  (:export #:main #:run-shell #:*history*))

(in-package #:sbsh)

;;; Global shell state.

(defvar *interactive* nil
  "True when the shell is attached to a controlling terminal.")

(defvar *shell-terminal* 0
  "File descriptor of the controlling terminal (stdin).")

(defvar *shell-pgid* nil
  "Process-group id of the shell itself.")

(defvar *shell-tmodes* nil
  "Saved terminal attributes (cooked mode) for the shell.")

(defvar *last-status* 0
  "Exit status of the most recently completed foreground command.")

(defvar *should-exit* nil
  "When set to an integer, the REPL loop terminates with that exit code.")
