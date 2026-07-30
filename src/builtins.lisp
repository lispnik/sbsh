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

(defun normalize-path (path)
  "Resolve . and .. components of an absolute PATH textually (without following
symlinks), so cd keeps the logical path like bash's default."
  (let ((stack '()))
    (dolist (p (split-on-char path #\/))
      (cond
        ((or (string= p "") (string= p ".")))
        ((string= p "..") (when stack (pop stack)))
        (t (push p stack))))
    (if stack
        (format nil "~{/~A~}" (nreverse stack))
        "/")))

(defun logical-path (target)
  "The logical absolute path of TARGET relative to $PWD."
  (normalize-path
   (if (and (plusp (length target)) (char= (char target 0) #\/))
       target
       (concatenate 'string (or (getenv "PWD") (sb-posix:getcwd)) "/" target))))

(defun update-cwd (logical physical)
  "Record LOGICAL as $PWD after having chdir'd; PHYSICAL is the fallback path."
  (setf *default-pathname-defaults*
        (pathname (concatenate 'string (string-right-trim "/" physical) "/")))
  (sb-posix:setenv "PWD" logical 1)
  logical)

(defun change-directory (target)
  (let* ((old (or (getenv "PWD") (ignore-errors (sb-posix:getcwd)) "/"))
         (dest (cond
                 ((null target) (or (getenv "HOME") "/"))
                 ((string= target "-")
                  (or (getenv "OLDPWD")
                      (progn (format *error-output* "cd: OLDPWD not set~%")
                             (return-from change-directory 1))))
                 (t (expand-tilde target))))
         (logical (logical-path dest)))
    (handler-case
        (progn
          ;; chdir to the logical path; fall back to the raw target if that
          ;; textual path does not exist (symlinked ..).
          (handler-case (sb-posix:chdir logical)
            (sb-posix:syscall-error () (sb-posix:chdir dest)
              (setf logical (sb-posix:getcwd))))
          (update-cwd logical (sb-posix:getcwd))
          (sb-posix:setenv "OLDPWD" old 1)
          (when (and target (string= target "-"))
            (format t "~A~%" logical))
          (run-cd-hooks logical)
          0)
      (sb-posix:syscall-error ()
        (format *error-output* "cd: ~A: No such file or directory~%" dest)
        1))))

(defun file-exists-p (path)
  "True if PATH exists, using stat(2) so characters like [ that are CL
pathname wildcards (e.g. the `[` command) are handled literally."
  (ignore-errors (sb-posix:stat path) t))

(defun file-executable-p (path)
  "True if PATH is executable by the current process."
  (ignore-errors (sb-posix:access path sb-posix:x-ok) t))

(defun path-search (name)
  "Return the full path of executable NAME found on $PATH, or NIL.
Names containing a slash are returned as-is if they exist."
  (if (find #\/ name)
      (and (file-exists-p name) name)
      (dolist (dir (split-on-char (or (getenv "PATH") "") #\:) nil)
        (when (plusp (length dir))
          (let ((candidate (format nil "~A/~A" (string-right-trim "/" dir) name)))
            (when (file-exists-p candidate)
              (return candidate)))))))

;;; --- Builtins -----------------------------------------------------------

(define-builtin "cd" (args)
  (change-directory (first args)))

(define-builtin "pwd" (args)
  (if (and args (string= (first args) "-P"))
      (format t "~A~%" (sb-posix:getcwd))               ; physical
      (format t "~A~%" (or (getenv "PWD") (sb-posix:getcwd))))  ; logical
  0)

(define-builtin "exit" (args)
  (let ((code (cond
                ((null args) *last-status*)
                (t (let ((n (parse-integer (first args) :junk-allowed t)))
                     (cond
                       (n (logand n #xff))     ; exit status is mod 256
                       (t (format *error-output* "exit: ~A: numeric argument required~%"
                                  (first args))
                          2)))))))
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
              (sb-posix:setenv (subseq a 0 eq)
                               (expand-assignment-value (subseq a (1+ eq))) 1))))
        0)))

(define-builtin "unset" (args)
  (let ((funcs nil))
    (when (and args (string= (first args) "-f")) (setf funcs t args (rest args)))
    (dolist (a args)
      (if funcs (remhash a *functions*) (env-unset a))))
  0)

(define-builtin "env" (args)
  (dolist (kv (sb-ext:posix-environ)) (format t "~A~%" kv))
  0)

(defun set-o-option (name on)
  "Set a long-named shell option; returns NIL for an unknown name (so `set`
can report failure and exit non-zero)."
  (cond
    ((string= name "pipefail") (setf *pipefail* on) t)
    ((string= name "errexit") (setf *errexit* on) t)
    ((string= name "nounset") (setf *nounset* on) t)
    ((string= name "noclobber") (setf *noclobber* on) t)
    ((string= name "noglob") (setf *noglob* on) t)
    ((string= name "xtrace") (setf *xtrace* on) t)
    (t (format *error-output* "set: ~A: invalid option name~%" name) nil)))

(defun print-set-options ()
  (format t "pipefail~vT~A~%"  12 (if *pipefail* "on" "off"))
  (format t "errexit~vT~A~%"   12 (if *errexit* "on" "off"))
  (format t "nounset~vT~A~%"   12 (if *nounset* "on" "off"))
  (format t "noclobber~vT~A~%" 12 (if *noclobber* "on" "off"))
  (format t "noglob~vT~A~%"    12 (if *noglob* "on" "off"))
  (format t "xtrace~vT~A~%"    12 (if *xtrace* "on" "off")))

(define-builtin "set" (args)
  (cond
    ((null args)
     (dolist (kv (sort (copy-list (sb-ext:posix-environ)) #'string<))
       (format t "~A~%" kv))
     0)
    (t
     (let ((status 0))
       (loop while args do
         (let ((tok (pop args)))
           (cond
             ((string= tok "--") (setf *positional* (copy-list args)) (setf args nil))
             ((or (string= tok "-o") (string= tok "+o"))
              (if args
                  (unless (set-o-option (pop args) (string= tok "-o")) (setf status 1))
                  (print-set-options)))
             ((and (plusp (length tok)) (member (char tok 0) '(#\- #\+)))
              (let ((on (char= (char tok 0) #\-)))
                (loop for c across (subseq tok 1) do
                  (case c
                    (#\e (setf *errexit* on))
                    (#\u (setf *nounset* on))
                    (#\x (setf *xtrace* on))
                    (#\f (setf *noglob* on))
                    (#\C (setf *noclobber* on))
                    (t (format *error-output* "set: -~A: invalid option~%" c)
                       (setf status 1))))))
             (t (setf *positional* (cons tok args)) (setf args nil)))))
       status))))

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
      ((shell-function name) (format t "~A is a function~%" name))
      ((nth-value 1 (gethash name *aliases*))
       (format t "~A is aliased to '~{~A~^ ~}'~%" name (gethash name *aliases*)))
      ((builtin-p name) (format t "~A is a shell builtin~%" name))
      ((path-search name) (format t "~A is ~A~%" name (path-search name)))
      (t (format t "~A: not found~%" name))))
  0)

(defun env-unset (name)
  "Unset environment variable NAME (falling back to empty when unsupported)."
  (handler-case (sb-posix:unsetenv name)
    (error () (ignore-errors (sb-posix:setenv name "" 1)))))

(define-builtin "return" (args)
  (if (not *in-function*)
      (progn (format *error-output* "return: can only `return' from a function~%") 1)
      (throw 'sbsh-return
        (if args (or (parse-integer (first args) :junk-allowed t) 0) *last-status*))))

(define-builtin "local" (args)
  (if (not *in-function*)
      (progn (format *error-output* "local: can only be used in a function~%") 1)
      (progn
        (dolist (a args)
          (let* ((eq (position #\= a))
                 (name (if eq (subseq a 0 eq) a))
                 (old (sb-posix:getenv name)))
            ;; Save the shadowed value; restored when the function returns.
            (push (if old
                      (lambda () (sb-posix:setenv name old 1))
                      (lambda () (env-unset name)))
                  *function-local-restores*)
            (sb-posix:setenv name (if eq (subseq a (1+ eq)) "") 1)))
        0)))

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

(define-builtin "eval" (args)
  "Concatenate ARGS with spaces and execute the result as shell input."
  (when args
    (run-command-string (format nil "~{~A~^ ~}" args)))
  *last-status*)

(defun source-file (path pos-args)
  "Execute the commands in the file at PATH in the current shell environment."
  (if (null path)
      (progn (format *error-output* "sbsh: .: filename argument required~%") 2)
      (let ((full (if (find #\/ path) path (or (path-search path) path)))
            ;; `. file a b c` sets the positional parameters for the script.
            (*positional* (if pos-args pos-args *positional*)))
        (handler-case
            (with-open-file (in full :external-format :utf-8)
              (run-lines (lambda () (read-line in nil :eof)))
              *last-status*)
          (file-error ()
            (format *error-output* "sbsh: .: ~A: No such file or directory~%" path)
            1)))))

(define-builtin "." (args) (source-file (first args) (rest args)))
(define-builtin "source" (args) (source-file (first args) (rest args)))

(define-builtin "exec" (args)
  "exec CMD ARGS...: replace the shell with CMD.  With no command (only
redirections), the redirections are applied to the shell permanently -- handled
in RUN-STANDALONE-BUILTIN before we get here."
  (if (null args)
      0
      (let ((path (path-search (first args))))
        (finish-output)
        (cond
          ((null path)
           (format *error-output* "sbsh: exec: ~A: not found~%" (first args))
           (setf *should-exit* 127)
           127)
          (t (%execvp path (build-argv args))       ; only returns on failure
             (let ((code (if (file-executable-p path) 127 126)))
               (format *error-output* "sbsh: exec: ~A: ~A~%" (first args)
                       (if (= code 126) "Permission denied" "not found"))
               (finish-output *error-output*)
               (sb-ext:exit :code code :abort t)))))))

(define-builtin "readonly" (args)
  (cond
    ((null args)
     (maphash (lambda (k v) (declare (ignore v)) (format t "readonly ~A~%" k))
              *readonly-vars*)
     0)
    (t (dolist (a args)
         (let ((eq (position #\= a)))
           (if eq
               (let ((name (subseq a 0 eq)))
                 (sb-posix:setenv name (expand-assignment-value (subseq a (1+ eq))) 1)
                 (setf (gethash name *readonly-vars*) t))
               (setf (gethash a *readonly-vars*) t))))
       0)))

(define-builtin "shift" (args)
  (let ((n (if args (parse-integer (first args) :junk-allowed t) 1)))
    (cond
      ((or (null n) (minusp n))
       (format *error-output* "shift: ~A: bad shift count~%" (first args))
       1)
      ((> n (length *positional*)) 1)          ; over-shift: no change, fail
      (t (setf *positional* (nthcdr n *positional*)) 0))))

(defun assign-read-vars (vars line &optional raw-p)
  "Assign LINE to VARS per IFS.  The first VARS get one field each; the last var
gets the unsplit remainder (trailing IFS whitespace trimmed).  Unless RAW-P, a
backslash quotes the next character so an escaped IFS char does not split (#40).
IFS whitespace adjacent to an IFS non-whitespace delimiter is absorbed (#41)."
  (let* ((ifs (ifs-value)) (nvars (length vars)) (n (length line)) (i 0))
    (when (zerop (length ifs))          ; empty IFS: no splitting at all
      (sb-posix:setenv (first vars) (if raw-p line (remove-backslashes line)) 1)
      (dolist (v (rest vars)) (sb-posix:setenv v "" 1))
      (return-from assign-read-vars))
    (let ((ifs-ws (remove-if-not (lambda (c) (member c '(#\Space #\Tab #\Newline)))
                                 (coerce ifs 'list))))
     (labels ((ws-p (c) (and (member c '(#\Space #\Tab #\Newline)) (find c ifs)))
             (nonws-p (c) (and (find c ifs) (not (member c '(#\Space #\Tab #\Newline)))))
             (skip-ws () (loop while (and (< i n) (ws-p (char line i))) do (incf i)))
             ;; Consume one logical delimiter: IFS whitespace, plus at most one
             ;; IFS non-whitespace char, with surrounding whitespace absorbed.
             (skip-delim ()
               (skip-ws)
               (when (and (< i n) (nonws-p (char line i))) (incf i) (skip-ws)))
             (read-field ()
               (with-output-to-string (out)
                 (loop while (< i n) do
                   (let ((c (char line i)))
                     (cond
                       ((and (not raw-p) (char= c #\\) (< (1+ i) n))
                        (write-char (char line (1+ i)) out) (incf i 2))  ; escaped
                       ((find c ifs) (return))
                       (t (write-char c out) (incf i))))))))
      (skip-ws)                         ; trim leading IFS whitespace
      (loop for vi from 0 for v in vars do
        (if (= vi (1- nvars))
            ;; last var: remainder (escapes processed, trailing IFS ws trimmed)
            (let ((rest (with-output-to-string (out)
                          (loop while (< i n) do
                            (let ((c (char line i)))
                              (cond
                                ((and (not raw-p) (char= c #\\) (< (1+ i) n))
                                 (write-char (char line (1+ i)) out) (incf i 2))
                                (t (write-char c out) (incf i))))))))
              (sb-posix:setenv v (string-right-trim ifs-ws rest) 1))
            (progn (sb-posix:setenv v (read-field) 1) (skip-delim))))
      ;; If the line ran out before the last var, remaining vars are empty.
      nil))))

(defun remove-backslashes (s)
  "Remove backslash escapes from S (each \\X -> X), for non-raw read."
  (if (find #\\ s)
      (with-output-to-string (out)
        (let ((i 0) (n (length s)))
          (loop while (< i n) do
            (if (and (char= (char s i) #\\) (< (1+ i) n))
                (progn (write-char (char s (1+ i)) out) (incf i 2))
                (progn (write-char (char s i) out) (incf i))))))
      s))

(defun odd-trailing-backslashes-p (line)
  "True if LINE ends with an odd number of backslashes (so the last one is a
line-continuation, not an escaped backslash)."
  (let ((k 0))
    (loop for i downfrom (1- (length line)) to 0
          while (char= (char line i) #\\) do (incf k))
    (oddp k)))

(defun read-input-line (raw-p)
  "Read one input line for `read`.  Unless RAW-P, a trailing backslash continues
onto the next line (the backslash-newline is removed); other backslash escapes
are left in place for ASSIGN-READ-VARS to process during field splitting.
Returns (values LINE MISSING-NEWLINE); LINE is NIL at end of input."
  (multiple-value-bind (line missing) (read-line (tty-in) nil nil)
    (cond
      ((null line) (values nil t))
      (raw-p (values line missing))
      (t (let ((out (make-string-output-stream)))
           (loop
             (if (odd-trailing-backslashes-p line)
                 (progn
                   (write-string (subseq line 0 (1- (length line))) out)  ; drop trailing \
                   (multiple-value-bind (nl nm) (read-line (tty-in) nil nil)
                     (if (null nl) (progn (setf missing t) (return))
                         (setf line nl missing nm))))
                 (progn (write-string line out) (return))))
           (values (get-output-stream-string out) missing))))))

(define-builtin "read" (args)
  "read [-r] [VAR...] -- read a line of stdin into variables (REPLY by default).
Without -r, backslash escapes and line continuation are processed.  Returns 1
at end of input (a final line with no newline still assigns)."
  (let ((raw-p (and args (string= (first args) "-r"))))
    (when raw-p (setf args (rest args)))
    (multiple-value-bind (line missing-newline) (read-input-line raw-p)
      (if (null line)
          1                             ; end of input, nothing read
          (progn (assign-read-vars (or args (list "REPLY")) line raw-p)
                 (if missing-newline 1 0))))))

(defun normalize-trap-name (spec)
  "Canonical trap condition name for SPEC: EXIT (or 0), or a signal short name
like INT / TERM (SIG-prefix and numbers accepted)."
  (let ((up (string-upcase spec)))
    (cond
      ((or (string= up "EXIT") (string= up "0")) "EXIT")
      (t (let ((num (signal-number spec)))
           (if num
               (or (car (rassoc num (signal-name-alist))) up)
               (if (starts-with-subseq "SIG" up) (subseq up 3) up)))))))

(defun signal-name-alist ()
  (list (cons "HUP" sb-posix:sighup) (cons "INT" sb-posix:sigint)
        (cons "QUIT" sb-posix:sigquit) (cons "TERM" sb-posix:sigterm)
        (cons "USR1" sb-posix:sigusr1) (cons "USR2" sb-posix:sigusr2)
        (cons "ALRM" sb-posix:sigalrm)))

(defun install-trap-handler (name)
  "Install a signal handler that runs the trap action registered for NAME."
  (let ((num (signal-number name)))
    (when num
      (ignore-errors
       (sb-sys:enable-interrupt
        num (lambda (&rest _)
              (declare (ignore _))
              (let ((action (gethash name *traps*)))
                (when action (ignore-errors (run-command-string action))))))))))

(defun run-exit-trap ()
  "Run the EXIT trap action, if any (called just before the shell exits).  The
shell's exit status is preserved across the trap unless the trap itself calls
`exit` (POSIX)."
  (let ((action (gethash "EXIT" *traps*)))
    (when action
      (remhash "EXIT" *traps*)            ; run at most once
      (let ((saved *last-status*) (trap-exit nil))
        ;; Bind *should-exit* off so the trap body actually runs even though we
        ;; are on our way out; capture whether the trap itself called `exit`.
        (let ((*should-exit* nil))
          (ignore-errors (run-command-string action))
          (setf trap-exit *should-exit*))
        (setf *last-status* (or trap-exit saved))
        (when trap-exit (setf *should-exit* trap-exit))))))

(define-builtin "trap" (args)
  (cond
    ((or (null args) (and (string= (first args) "-p") (null (rest args))))
     (maphash (lambda (k v) (format t "trap -- '~A' ~A~%" v k)) *traps*)
     0)
    (t (let ((action (first args)) (names (rest args)))
         ;; `trap - SIG` (or an empty/`-` action) resets the trap.
         (dolist (spec names)
           (let ((name (normalize-trap-name spec)))
             (cond
               ((string= action "-")
                (remhash name *traps*)
                (let ((num (signal-number name)))
                  (when num (ignore-errors (sb-sys:enable-interrupt num :default)))))
               (t (setf (gethash name *traps*) action)
                  (unless (string= name "EXIT") (install-trap-handler name))))))
         0))))

;;; --- getopts -----------------------------------------------------------

(defvar *getopts-offset* 0
  "Char index of the next option letter within the current arg (0 = start of a
fresh arg).  Tracks position through a bundled option group like -abc.")
(defvar *getopts-last-optind* 0
  "The OPTIND value getopts last set, to detect an external reset.")

(defun run-getopts (optstring name params)
  "One step of POSIX getopts over PARAMS, using/updating $OPTIND and $OPTARG and
storing the found option letter in NAME.  Returns 0 while options remain, 1 when
done.  A leading ':' in OPTSTRING selects silent error reporting."
  (let* ((silent (and (plusp (length optstring)) (char= (char optstring 0) #\:)))
         (optind (or (parse-integer (or (getenv "OPTIND") "1") :junk-allowed t) 1)))
    (when (/= optind *getopts-last-optind*) (setf *getopts-offset* 0))  ; external reset
    (labels ((done (oi) (sb-posix:setenv "OPTIND" (princ-to-string oi) 1)
                        (setf *getopts-last-optind* oi *getopts-offset* 0) 1)
             (found (oi code) (sb-posix:setenv "OPTIND" (princ-to-string oi) 1)
                              (setf *getopts-last-optind* oi) code)
             (takes-arg (ch)
               (let ((p (position ch optstring)))
                 (and p (< (1+ p) (length optstring)) (char= (char optstring (1+ p)) #\:))))
             (known (ch) (and (char/= ch #\:) (find ch optstring))))
      (loop
        (when (> optind (length params)) (return (done optind)))
        (let ((cur (nth (1- optind) params)))
          (when (<= *getopts-offset* 0)                 ; validate start of a new arg
            (cond
              ((or (zerop (length cur)) (char/= (char cur 0) #\-) (string= cur "-"))
               (return (done optind)))
              ((string= cur "--") (return (done (1+ optind))))
              (t (setf *getopts-offset* 1))))
          (let* ((ch (char cur *getopts-offset*)))
            (incf *getopts-offset*)
            (let ((at-end (>= *getopts-offset* (length cur))))
              (when at-end (setf *getopts-offset* 0))
              (flet ((next-arg () (if at-end (1+ optind) optind)))
                (cond
                  ((not (known ch))                     ; unknown option
                   (if silent
                       (progn (sb-posix:setenv name "?" 1) (sb-posix:setenv "OPTARG" (string ch) 1))
                       (progn (format *error-output* "getopts: illegal option -- ~C~%" ch)
                              (sb-posix:setenv name "?" 1) (sb-posix:setenv "OPTARG" "" 1)))
                   (return (found (next-arg) 0)))
                  ((takes-arg ch)                       ; option that needs an argument
                   (cond
                     ((not at-end)                      ; arg = rest of this arg
                      (sb-posix:setenv "OPTARG" (subseq cur *getopts-offset*) 1)
                      (sb-posix:setenv name (string ch) 1)
                      (setf *getopts-offset* 0)
                      (return (found (1+ optind) 0)))
                     ((< optind (length params))        ; arg = the next parameter
                      (sb-posix:setenv "OPTARG" (nth optind params) 1)
                      (sb-posix:setenv name (string ch) 1)
                      (return (found (+ optind 2) 0)))
                     (silent                            ; missing arg, silent
                      (sb-posix:setenv name ":" 1) (sb-posix:setenv "OPTARG" (string ch) 1)
                      (return (found (1+ optind) 0)))
                     (t                                 ; missing arg, verbose
                      (format *error-output* "getopts: option requires an argument -- ~C~%" ch)
                      (sb-posix:setenv name "?" 1) (sb-posix:setenv "OPTARG" "" 1)
                      (return (found (1+ optind) 0)))))
                  (t                                    ; plain option, no argument
                   (sb-posix:setenv name (string ch) 1)
                   (sb-posix:setenv "OPTARG" "" 1)
                   (return (found (next-arg) 0))))))))))))

(define-builtin "getopts" (args)
  (if (< (length args) 2)
      (progn (format *error-output* "getopts: usage: getopts optstring name [arg ...]~%") 2)
      (run-getopts (first args) (second args)
                   (if (cddr args) (cddr args) *positional*))))

(define-builtin "wait" (args)
  "Block until all child processes have finished."
  (declare (ignore args))
  (handler-case
      (loop (multiple-value-bind (pid status) (sb-posix:waitpid -1 0)
              (if (or (null pid) (<= pid 0))
                  (return)
                  (mark-process-status pid status))))
    (sb-posix:syscall-error () nil))    ; ECHILD: nothing left to wait for
  (setf *jobs* (remove-if #'job-completed-p *jobs*))
  0)

;;; --- test / [ -----------------------------------------------------------

(defun stat-mode (path) (ignore-errors (sb-posix:stat-mode (sb-posix:stat path))))
(defun test-regular-p (path)
  (let ((m (stat-mode path))) (and m (= (logand m #o170000) #o100000))))
(defun test-dir-p (path)
  (let ((m (stat-mode path))) (and m (= (logand m #o170000) #o040000))))
(defun test-nonempty-p (path)
  (let ((s (ignore-errors (sb-posix:stat path))))
    (and s (plusp (sb-posix:stat-size s)))))

(defun parse-int-strict (s)
  "Parse S as an integer only if the WHOLE token is numeric (optionally signed,
surrounding blanks allowed).  Returns NIL for junk like \"3x\" -- unlike
PARSE-INTEGER :junk-allowed, which would silently accept the leading 3."
  (multiple-value-bind (n end) (parse-integer s :junk-allowed t)
    (and n (= end (length (string-right-trim '(#\Space #\Tab) s))) n)))

(defun test-int (a op b cmp)
  (declare (ignore op))
  (let ((x (parse-int-strict a))
        (y (parse-int-strict b)))
    (if (and x y)
        (if (funcall cmp x y) 0 1)
        (progn (format *error-output* "[: integer expression expected~%") 2))))

(defun test-binary-op-p (op)
  (member op '("=" "==" "!=" "-eq" "-ne" "-lt" "-le" "-gt" "-ge"
               "-nt" "-ot" "-ef")
          :test #'string=))

(defun file-stat (path) (ignore-errors (sb-posix:stat path)))
(defun file-lstat (path) (ignore-errors (sb-posix:lstat path)))
(defun mode-type= (path type-bits &optional lstat)
  (let ((s (if lstat (file-lstat path) (file-stat path))))
    (and s (= (logand (sb-posix:stat-mode s) #o170000) type-bits))))

(defun test-binary (a op b)
  (cond
    ((member op '("=" "==") :test #'string=) (if (string= a b) 0 1))
    ((string= op "!=") (if (string/= a b) 0 1))
    ((string= op "-eq") (test-int a op b #'=))
    ((string= op "-ne") (test-int a op b #'/=))
    ((string= op "-lt") (test-int a op b #'<))
    ((string= op "-le") (test-int a op b #'<=))
    ((string= op "-gt") (test-int a op b #'>))
    ((string= op "-ge") (test-int a op b #'>=))
    ((string= op "-nt")                       ; a newer than b
     (let ((sa (file-stat a)) (sb (file-stat b)))
       (if (and sa (or (null sb) (> (sb-posix:stat-mtime sa) (sb-posix:stat-mtime sb)))) 0 1)))
    ((string= op "-ot")                       ; a older than b
     (let ((sa (file-stat a)) (sb (file-stat b)))
       (if (and sb (or (null sa) (< (sb-posix:stat-mtime sa) (sb-posix:stat-mtime sb)))) 0 1)))
    ((string= op "-ef")                       ; same device + inode
     (let ((sa (file-stat a)) (sb (file-stat b)))
       (if (and sa sb (= (sb-posix:stat-dev sa) (sb-posix:stat-dev sb))
                (= (sb-posix:stat-ino sa) (sb-posix:stat-ino sb))) 0 1)))
    (t (format *error-output* "[: ~A: unknown operator~%" op) 2)))

(defun test-unary (op a)
  (flet ((b (x) (if x 0 1)))
    (cond
      ((string= op "-z") (b (zerop (length a))))
      ((string= op "-n") (b (plusp (length a))))
      ((string= op "-e") (b (file-exists-p a)))
      ((string= op "-f") (b (test-regular-p a)))
      ((string= op "-d") (b (test-dir-p a)))
      ((string= op "-s") (b (test-nonempty-p a)))
      ((member op '("-L" "-h") :test #'string=) (b (mode-type= a #o120000 t))) ; symlink
      ((string= op "-p") (b (mode-type= a #o010000)))     ; fifo
      ((string= op "-S") (b (mode-type= a #o140000)))     ; socket
      ((string= op "-b") (b (mode-type= a #o060000)))     ; block device
      ((string= op "-c") (b (mode-type= a #o020000)))     ; char device
      ((member op '("-r" "-w" "-x") :test #'string=) (b (file-exists-p a)))
      (t (format *error-output* "[: ~A: unary operator expected~%" op) 2))))

(defun split-arg-list (list sep)
  "Split LIST into sublists at each element equal to SEP."
  (let ((parts '()) (cur '()))
    (dolist (x list) (if (string= x sep)
                         (progn (push (nreverse cur) parts) (setf cur '()))
                         (push x cur)))
    (push (nreverse cur) parts)
    (nreverse parts)))

(defun test-primary (args)
  (case (length args)
    (0 1)
    (1 (if (plusp (length (first args))) 0 1))
    (2 (if (string= (first args) "!")
           (if (zerop (test-primary (rest args))) 1 0)
           (test-unary (first args) (second args))))
    (3 (cond
         ((test-binary-op-p (second args))
          (test-binary (first args) (second args) (third args)))
         ((string= (first args) "!") (if (zerop (test-primary (rest args))) 1 0))
         (t (format *error-output* "[: ~A: unknown operator~%" (second args)) 2)))
    (t (if (string= (first args) "!")
           (if (zerop (test-primary (rest args))) 1 0)
           (progn (format *error-output* "[: too many arguments~%") 2)))))

;;; A small recursive-descent grammar so that ( ) grouping, ! negation, and the
;;; -a / -o connectives all compose (e.g. [ ( a = a -o b = c ) -a ! -z x ]).
;;;   or   := and  ( -o and )*
;;;   and  := term ( -a term )*
;;;   term := ! term | ( or ) | primary

(defun test-atom (args)
  "Consume one primary (up to the next -a / -o / ) or the end); return
(values CODE REMAINING-ARGS)."
  (let ((prim '()) (rest args))
    (loop while (and rest (not (member (first rest) '("-a" "-o" ")") :test #'string=)))
          do (push (pop rest) prim))
    (values (test-primary (nreverse prim)) rest)))

(defun test-term (args)
  (cond
    ((null args) (values 1 nil))
    ((string= (first args) "!")
     (multiple-value-bind (v rest) (test-term (rest args))
       (values (if (zerop v) 1 0) rest)))
    ((string= (first args) "(")
     (multiple-value-bind (v rest) (test-or-expr (rest args))
       (if (and rest (string= (first rest) ")"))
           (values v (rest rest))
           (progn (format *error-output* "[: missing `)'~%") (values 2 nil)))))
    (t (test-atom args))))

(defun test-and-expr (args)
  (multiple-value-bind (v rest) (test-term args)
    (loop while (and rest (string= (first rest) "-a"))
          do (multiple-value-bind (v2 r2) (test-term (rest rest))
               (setf v (if (and (zerop v) (zerop v2)) 0 1) rest r2)))
    (values v rest)))

(defun test-or-expr (args)
  (multiple-value-bind (v rest) (test-and-expr args)
    (loop while (and rest (string= (first rest) "-o"))
          do (multiple-value-bind (v2 r2) (test-and-expr (rest rest))
               (setf v (if (or (zerop v) (zerop v2)) 0 1) rest r2)))
    (values v rest)))

(defun shell-test (args)
  "Evaluate a test expression with ( ) grouping, ! negation, and -a / -o.
Plain 0-3 argument expressions bypass the grammar for POSIX corner-case
compatibility (e.g. [ ( ] tests the string \"(\")."
  (if (and (<= (length args) 3)
           (not (member ")" args :test #'string=)))
      (test-primary args)
      (multiple-value-bind (v rest) (test-or-expr args)
        (if rest
            (progn (format *error-output* "[: unexpected argument `~A'~%" (first rest)) 2)
            v))))

(define-builtin "test" (args) (shell-test args))

(define-builtin "[" (args)
  (if (and args (string= (car (last args)) "]"))
      (shell-test (butlast args))
      (progn (format *error-output* "[: missing `]'~%") 2)))

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
