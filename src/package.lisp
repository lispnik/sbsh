;;;; package.lisp --- Package definitions and global shell state.

(defpackage #:sbsh
  (:use #:cl)
  (:export #:main #:run-shell #:*history*
           ;; --- user-facing API (also usable from ~/.sbshrc) ---
           #:defalias #:unalias #:defcommand #:defprompt #:defcompletion
           #:on-cd #:sh #:sh-capture #:shell-eval #:cwd #:getenv #:setenv
           #:history-where #:command-used-p #:failed-p #:entry-text
           #:*aliases* #:*prompt-fn*))

;;; A separate package that ~/.sbshrc and interactive `(...)` Lisp escapes run
;;; in.  It sees CL plus the curated sbsh API, but not sbsh's internals.
(defpackage #:sbsh-user
  (:use #:cl)
  (:import-from #:sbsh
                #:defalias #:unalias #:defcommand #:defprompt #:defcompletion
                #:on-cd #:sh #:sh-capture #:shell-eval #:cwd #:getenv #:setenv
                #:history-where #:command-used-p #:failed-p #:entry-text))

(in-package #:sbsh)

;;; --- Global shell state -------------------------------------------------

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

;;; --- State backing the Common Lisp extensions ---------------------------

(defvar *aliases* (make-hash-table :test 'equal)
  "Map of alias name -> list of replacement words.")

(defvar *prompt-fn* nil
  "When set, a function of no arguments returning the prompt string.")

(defvar *completions* (make-hash-table :test 'equal)
  "Map of command name -> completion function of (WORD) -> list of strings.")

(defvar *cd-hooks* '()
  "Functions of one argument (the new directory) run after a successful cd.")

(defvar *functions* (make-hash-table :test 'equal)
  "Map of shell-function name -> SHFUN.")

(defvar *positional* '()
  "The current positional parameters ($1, $2, ...) inside a function.")

(defvar *in-function* nil
  "True while executing a shell function body (enables `return`/`local`).")

(defvar *function-local-restores* '()
  "Thunks that restore variables shadowed by `local`, run on function return.")

(defvar *loop-depth* 0
  "Current nesting depth of for/while/until loops (enables break/continue).")

(defvar *history-records* (make-array 0 :adjustable t :fill-pointer 0)
  "Structured, queryable history: one HIST-ENTRY per executed line.")

(defparameter *user-package* (find-package '#:sbsh-user)
  "The package interactive Lisp escapes and ~/.sbshrc evaluate in.")

;; Bound inside a Lisp pipeline stage: LINES is the list of input lines and
;; INPUT is the whole input string.  Interned in SBSH-USER so `lines` in a
;; user form such as `ls | (sort lines)` refers to them.
(defvar sbsh-user::lines nil)
(defvar sbsh-user::input "")

(defvar *capturing* nil
  "True while capturing output for command substitution (suppresses prompts).")

(defvar *line-commands* nil
  "Accumulates command descriptors of the current line, for history records.")

(defvar *heredoc-bodies* nil
  "Ordered heredoc bodies collected by the reader for the current line; each
<< redirection pops the next one during parsing.")

(defvar *heredoc-temps* nil
  "Temp files materialized for heredocs this line, deleted as a backstop.")
