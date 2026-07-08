;;;; builtins.lisp --- Shell built-in commands.

(in-package #:sbsh)

(defvar *builtins* (make-hash-table :test 'equal)
  "Map of builtin name -> function of (ARGS) returning an exit code.")

(defmacro define-builtin (name (args) &body body)
  "Define a builtin command NAME.  ARGS is bound to the argument list
(excluding argv[0]).  The body's value is the exit code."
  `(setf (gethash ,name *builtins*)
         (lambda (,args) (declare (ignorable ,args)) ,@body)))

(defun builtin-p (name)
  (nth-value 1 (gethash name *builtins*)))

(defun run-builtin (name args)
  (funcall (gethash name *builtins*) args))

;;; --- Filesystem / process helpers ---------------------------------------

(defun update-cwd (dir)
  "chdir to DIR (a namestring), updating *default-pathname-defaults* and PWD."
  (sb-posix:chdir dir)
  (let ((cwd (sb-posix:getcwd)))
    (setf *default-pathname-defaults*
          (pathname (concatenate 'string (string-right-trim "/" cwd) "/")))
    (sb-posix:setenv "PWD" cwd 1)
    cwd))

(defun change-directory (target)
  (let* ((old (or (ignore-errors (sb-posix:getcwd)) "/"))
         (dest (cond
                 ((null target) (or (getenv "HOME") "/"))
                 ((string= target "-")
                  (or (getenv "OLDPWD")
                      (progn (format *error-output* "cd: OLDPWD not set~%")
                             (return-from change-directory 1))))
                 (t (expand-tilde target)))))
    (handler-case
        (progn
          (update-cwd dest)
          (sb-posix:setenv "OLDPWD" old 1)
          (when (and target (string= target "-"))
            (format t "~A~%" (sb-posix:getcwd)))
          (run-cd-hooks (sb-posix:getcwd))
          0)
      (sb-posix:syscall-error ()
        (format *error-output* "cd: ~A: No such file or directory~%" dest)
        1))))

(defun path-search (name)
  "Return the full path of executable NAME found on $PATH, or NIL.
Names containing a slash are returned as-is if executable."
  (if (find #\/ name)
      (and (probe-file name) name)
      (dolist (dir (split-on-char (or (getenv "PATH") "") #\:) nil)
        (when (plusp (length dir))
          (let ((candidate (format nil "~A/~A" (string-right-trim "/" dir) name)))
            (when (probe-file candidate)
              (return candidate)))))))

;;; --- Builtins -----------------------------------------------------------

(define-builtin "cd" (args)
  (change-directory (first args)))

(define-builtin "pwd" (args)
  (format t "~A~%" (sb-posix:getcwd))
  0)

(define-builtin "exit" (args)
  (let ((code (if args (or (parse-integer (first args) :junk-allowed t) 0) *last-status*)))
    (setf *should-exit* code)
    code))

(define-builtin "echo" (args)
  (let ((newline t))
    (when (and args (string= (first args) "-n"))
      (setf newline nil args (rest args)))
    (format t "~{~A~^ ~}" args)
    (when newline (terpri))
    (force-output)
    0))

(define-builtin "export" (args)
  (if (null args)
      (progn (dolist (kv (sb-ext:posix-environ)) (format t "export ~A~%" kv)) 0)
      (progn
        (dolist (a args)
          (let ((eq (position #\= a)))
            (when eq
              (sb-posix:setenv (subseq a 0 eq) (subseq a (1+ eq)) 1))))
        0)))

(define-builtin "unset" (args)
  (dolist (a args)
    (handler-case (sb-posix:unsetenv a)
      (error () (ignore-errors (sb-posix:setenv a "" 1)))))
  0)

(define-builtin "env" (args)
  (dolist (kv (sb-ext:posix-environ)) (format t "~A~%" kv))
  0)

(define-builtin "set" (args)
  (dolist (kv (sort (copy-list (sb-ext:posix-environ)) #'string<))
    (format t "~A~%" kv))
  0)

(define-builtin "history" (args)
  (cond
    ((and args (string= (first args) "-c"))
     (history-clear) 0)
    (t (dotimes (i (history-count))
         (format t "~5D  ~A~%" (1+ i) (history-ref i)))
       0)))

(define-builtin "jobs" (args)
  (let ((current (current-job)))
    (dolist (job (reverse *jobs*))
      (format-job job *standard-output* current)))
  0)

(define-builtin "fg" (args)
  (let ((job (resolve-job-arg (first args))))
    (if job
        (progn (put-job-foreground job t) (job-exit-code job))
        (progn (format *error-output* "fg: no such job~%") 1))))

(define-builtin "bg" (args)
  (let ((job (resolve-job-arg (first args))))
    (if job
        (progn (put-job-background job t) 0)
        (progn (format *error-output* "bg: no such job~%") 1))))

(define-builtin "type" (args)
  (dolist (name args)
    (cond
      ((builtin-p name) (format t "~A is a shell builtin~%" name))
      ((path-search name) (format t "~A is ~A~%" name (path-search name)))
      (t (format t "~A: not found~%" name))))
  0)

(defun signal-number (spec)
  "Translate a signal SPEC like \"9\", \"KILL\", or \"SIGKILL\" to a number."
  (or (parse-integer spec :junk-allowed t)
      (let ((name (string-upcase spec)))
        (when (starts-with-subseq "SIG" name) (setf name (subseq name 3)))
        (cdr (assoc name
                    (list (cons "HUP" sb-posix:sighup) (cons "INT" sb-posix:sigint)
                          (cons "QUIT" sb-posix:sigquit) (cons "KILL" sb-posix:sigkill)
                          (cons "TERM" sb-posix:sigterm) (cons "STOP" sb-posix:sigstop)
                          (cons "CONT" sb-posix:sigcont) (cons "TSTP" sb-posix:sigtstp)
                          (cons "USR1" sb-posix:sigusr1) (cons "USR2" sb-posix:sigusr2))
                    :test #'string=)))))

(define-builtin "kill" (args)
  (let ((sig sb-posix:sigterm) (targets args))
    (when (and args (plusp (length (first args))) (char= (char (first args) 0) #\-))
      (setf sig (or (signal-number (subseq (first args) 1)) sb-posix:sigterm)
            targets (rest args)))
    (if (null targets)
        (progn (format *error-output* "kill: usage: kill [-SIG] %job|pid ...~%") 1)
        (progn
          (dolist (tgt targets)
            (if (and (plusp (length tgt)) (char= (char tgt 0) #\%))
                (let ((job (resolve-job-arg tgt)))
                  (if job
                      (progn (sb-posix:killpg (job-pgid job) sig)
                             (when (= sig sb-posix:sigcont) (mark-job-running job)))
                      (format *error-output* "kill: ~A: no such job~%" tgt)))
                (let ((pid (parse-integer tgt :junk-allowed t)))
                  (if pid
                      (handler-case (sb-posix:kill pid sig)
                        (sb-posix:syscall-error ()
                          (format *error-output* "kill: (~A): no such process~%" pid)))
                      (format *error-output* "kill: ~A: arguments must be pids or %job~%" tgt)))))
          0))))

(define-builtin "true" (args) 0)
(define-builtin ":" (args) 0)
(define-builtin "false" (args) 1)

(define-builtin "alias" (args)
  (cond
    ((null args)
     (let (names)
       (maphash (lambda (k v) (declare (ignore v)) (push k names)) *aliases*)
       (dolist (name (sort names #'string<))
         (format t "alias ~A='~{~A~^ ~}'~%" name (gethash name *aliases*))))
     0)
    (t
     (dolist (a args)
       (let ((eq (position #\= a)))
         (if eq
             (defalias (subseq a 0 eq) (subseq a (1+ eq)))
             (if (nth-value 1 (gethash a *aliases*))
                 (format t "alias ~A='~{~A~^ ~}'~%" a (gethash a *aliases*))
                 (format *error-output* "alias: ~A: not found~%" a)))))
     0)))

(define-builtin "unalias" (args)
  (dolist (a args) (unalias a))
  0)

(define-builtin "snapshot" (args)
  "Dump the live shell -- with everything defined this session -- to an
executable image, then exit.  Demonstrates image-based shells."
  (let ((path (or (first args) "sbsh-snapshot")))
    (format t "Saving shell image to ~A ...~%" path)
    (finish-output)
    (save-history)
    ;; save-lisp-and-die ends the process; the new image restarts at MAIN.
    (sb-ext:save-lisp-and-die path :executable t :toplevel #'main
                                   :save-runtime-options t)
    0))

(define-builtin "help" (args)
  (format t "sbsh --- a Common Lisp Unix shell~%~%Built-in commands:~%")
  (let ((names (sort (loop for k being the hash-keys of *builtins* collect k)
                     #'string<)))
    (format t "~{  ~A~%~}" names))
  (format t "~%Features: pipelines (|), redirections (< > >> 2> 2>&1),~%")
  (format t "  logical operators (&& ||), sequencing (;), background (&),~%")
  (format t "  job control (fg/bg/jobs, C-z to suspend), globbing (* ? []),~%")
  (format t "  variable and ~~ expansion, and an editing line reader with~%")
  (format t "  history (up/down, C-r search) and Tab completion.~%")
  0)

(defun resolve-job-arg (arg)
  "Resolve a job spec like \"%1\" or \"1\", or the current job when ARG is NIL."
  (cond
    ((null arg) (current-job))
    (t (let* ((s (if (and (plusp (length arg)) (char= (char arg 0) #\%))
                     (subseq arg 1) arg))
              (id (parse-integer s :junk-allowed t)))
         (and id (find-job id))))))
