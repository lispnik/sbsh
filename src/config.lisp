;;;; config.lisp --- The user-facing API and ~/.sbshrc DSL.
;;;;
;;;; Everything here is exported from SBSH and imported into SBSH-USER, so it
;;;; is available both interactively (via `(...)` Lisp escapes) and in the
;;;; ~/.sbshrc init file, which is ordinary Common Lisp.

(in-package #:sbsh)

;;; --- Small helpers exposed to user code ---------------------------------

(defun cwd ()
  "The current working directory as a string."
  (sb-posix:getcwd))

(defun setenv (name value)
  "Set environment variable NAME to VALUE (coerced to a string)."
  (sb-posix:setenv (string name) (princ-to-string value) 1)
  value)

(defun sh (line)
  "Run shell command LINE and return its exit status."
  (execute-line line))

(defun shell-eval (line)
  "Alias for SH: run a shell command line, returning its exit status."
  (execute-line line))

(defun sh-capture (line)
  "Run shell command LINE and return its stdout as a string."
  (capture-command-output line))

;;; --- Aliases ------------------------------------------------------------

(defun defalias (name expansion)
  "Define alias NAME (a string) that expands to the words of EXPANSION.
Simple word-level aliases only (no embedded pipes or operators)."
  (setf (gethash name *aliases*)
        (mapcar #'word-text (remove-if-not #'word-p (tokenize expansion))))
  name)

(defun unalias (name)
  "Remove the alias NAME."
  (remhash name *aliases*))

;;; --- Builtins defined in Lisp -------------------------------------------

(defmacro defcommand (name (args) &body body)
  "Define a shell builtin NAME implemented in Lisp.  ARGS is bound to the
argument list (strings).  The body's value, if an integer, is the exit code."
  `(progn
     (setf (gethash ,name *builtins*)
           (lambda (,args)
             (declare (ignorable ,args))
             (let ((result (progn ,@body)))
               (if (integerp result) result 0))))
     ,name))

;;; --- Prompt -------------------------------------------------------------

(defmacro defprompt ((&rest lambda-list) &body body)
  "Install a custom prompt.  The body must return the prompt string."
  `(setf *prompt-fn* (lambda (,@lambda-list) ,@body)))

;;; --- Per-command completion ---------------------------------------------

(defmacro defcompletion (name (word) &body body)
  "Register a completion for command NAME.  WORD is bound to the partial token;
the body returns a list of candidate strings."
  `(progn
     (setf (gethash ,name *completions*)
           (lambda (,word) (declare (ignorable ,word)) ,@body))
     ,name))

;;; --- Hooks --------------------------------------------------------------

(defun on-cd (function)
  "Register FUNCTION (of one argument, the new directory) to run after cd."
  (push function *cd-hooks*)
  function)

(defun run-cd-hooks (dir)
  (dolist (fn *cd-hooks*)
    (ignore-errors (funcall fn dir))))

;;; --- Loading the init file ----------------------------------------------

(defun rc-file ()
  (merge-pathnames ".sbshrc" (user-homedir-pathname)))

(defun load-rc-file ()
  "Load ~/.sbshrc as Common Lisp in the SBSH-USER package, if it exists."
  (let ((rc (rc-file)))
    (when (probe-file rc)
      (handler-case
          (let ((*package* *user-package*))
            (load rc :verbose nil :print nil))
        (error (e)
          (format *error-output* "sbsh: error loading ~A: ~A~%" rc e))))))
