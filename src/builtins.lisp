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
  (cond
    ((string= name "pipefail") (setf *pipefail* on))
    ((string= name "errexit") (setf *errexit* on))
    ((string= name "nounset") (setf *nounset* on))
    (t (format *error-output* "set: ~A: invalid option name~%" name))))

(defun print-set-options ()
  (format t "pipefail~vT~A~%" 12 (if *pipefail* "on" "off"))
  (format t "errexit~vT~A~%"  12 (if *errexit* "on" "off"))
  (format t "nounset~vT~A~%"  12 (if *nounset* "on" "off")))

(define-builtin "set" (args)
  (cond
    ((null args)
     (dolist (kv (sort (copy-list (sb-ext:posix-environ)) #'string<))
       (format t "~A~%" kv))
     0)
    (t
     (loop while args do
       (let ((tok (pop args)))
         (cond
           ((string= tok "--") (setf *positional* (copy-list args)) (setf args nil))
           ((or (string= tok "-o") (string= tok "+o"))
            (if args (set-o-option (pop args) (string= tok "-o")) (print-set-options)))
           ((and (plusp (length tok)) (member (char tok 0) '(#\- #\+)))
            (let ((on (char= (char tok 0) #\-)))
              (loop for c across (subseq tok 1) do
                (case c
                  (#\e (setf *errexit* on))
                  (#\u (setf *nounset* on))
                  (#\x nil)))))       ; xtrace: accepted, not implemented
           (t (setf *positional* (cons tok args)) (setf args nil)))))
     0)))

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

(define-builtin "shift" (args)
  (let ((n (if args (or (parse-integer (first args) :junk-allowed t) 1) 1)))
    (if (<= n (length *positional*))
        (progn (setf *positional* (nthcdr n *positional*)) 0)
        1)))

(defun assign-read-vars (vars line)
  "Assign LINE to VARS per IFS: the first VARS get one field each; the last var
gets the unsplit remainder (internal whitespace preserved, ends trimmed)."
  (let* ((ifs (ifs-value)) (n (length vars)) (len (length line)) (i 0))
    (when (zerop (length ifs))          ; empty IFS: no splitting at all
      (sb-posix:setenv (first vars) line 1)
      (dolist (v (rest vars)) (sb-posix:setenv v "" 1))
      (return-from assign-read-vars))
    (labels ((ws-p (c) (and (member c '(#\Space #\Tab #\Newline)) (find c ifs)))
             (delim-p (c) (find c ifs))
             (nonws-p (c) (and (not (member c '(#\Space #\Tab #\Newline))) (find c ifs))))
      (loop while (and (< i len) (ws-p (char line i))) do (incf i))   ; trim leading
      (loop for vi from 0 for v in vars do
        (cond
          ((= vi (1- n))
           ;; last var: the remainder with trailing IFS characters trimmed
           (let ((end len))
             (loop while (and (> end i) (delim-p (char line (1- end)))) do (decf end))
             (sb-posix:setenv v (subseq line i (max i end)) 1)))
          (t (let ((start i))
               (loop while (and (< i len) (not (delim-p (char line i)))) do (incf i))
               (sb-posix:setenv v (subseq line start i) 1)
               (if (and (< i len) (nonws-p (char line i)))
                   (progn (incf i) (loop while (and (< i len) (ws-p (char line i))) do (incf i)))
                   (loop while (and (< i len) (ws-p (char line i))) do (incf i))))))))))

(defun read-input-line (raw-p)
  "Read one input line for `read`.  Unless RAW-P, backslash escapes the next
character and a trailing backslash continues onto the next line.  Returns
(values LINE MISSING-NEWLINE); LINE is NIL at end of input."
  (multiple-value-bind (line missing) (read-line (tty-in) nil nil)
    (cond
      ((null line) (values nil t))
      (raw-p (values line missing))
      (t (let ((out (make-string-output-stream)))
           (loop
             (let ((i 0) (n (length line)) (cont nil))
               (loop while (< i n) do
                 (let ((c (char line i)))
                   (cond
                     ((and (char= c #\\) (= i (1- n))) (setf cont t) (incf i))
                     ((and (char= c #\\) (< (1+ i) n))
                      (write-char (char line (1+ i)) out) (incf i 2))
                     (t (write-char c out) (incf i)))))
               (if cont
                   (multiple-value-bind (nl nm) (read-line (tty-in) nil nil)
                     (if (null nl) (progn (setf missing t) (return))
                         (setf line nl missing nm)))
                   (return))))
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
          (progn (assign-read-vars (or args (list "REPLY")) line)
                 (if missing-newline 1 0))))))

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

(defun test-int (a op b cmp)
  (let ((x (parse-integer a :junk-allowed t))
        (y (parse-integer b :junk-allowed t)))
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

(defun shell-test (args)
  "Evaluate a test expression, honoring -o (OR) / -a (AND) between primaries."
  (if (not (or (member "-a" args :test #'string=) (member "-o" args :test #'string=)))
      (test-primary args)
      (let ((any nil))
        (dolist (or-part (split-arg-list args "-o"))
          (let ((all t))
            (dolist (and-part (split-arg-list or-part "-a"))
              (unless (zerop (test-primary and-part)) (setf all nil)))
            (when all (setf any t))))
        (if any 0 1))))

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
