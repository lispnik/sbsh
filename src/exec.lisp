;;;; exec.lisp --- Executing pipelines with real job control.
;;;;
;;;; This follows the classic Unix shell design: each pipeline becomes a
;;;; process group; the shell hands the terminal to the foreground group with
;;;; tcsetpgrp and reclaims it when the job stops or finishes.

(in-package #:sbsh)

(defparameter *jobctl-signals*
  (list sb-posix:sigint sb-posix:sigquit sb-posix:sigtstp
        sb-posix:sigttin sb-posix:sigttou)
  "Signals the interactive shell ignores and children reset to default.")

(defun init-job-control ()
  "Prepare the shell for job control: take our own process group, grab the
controlling terminal, and ignore job-control signals."
  (setf *shell-terminal* 0
        *interactive* (tty-p *shell-terminal*))
  (when *interactive*
    ;; If we are not in the foreground, stop until we are.
    (loop while (/= (tcgetpgrp *shell-terminal*) (sb-posix:getpgrp))
          do (sb-posix:kill (- (sb-posix:getpgrp)) sb-posix:sigttin))
    (dolist (sig *jobctl-signals*)
      (sb-sys:enable-interrupt sig :ignore))
    (setf *shell-pgid* (sb-posix:getpid))
    (ignore-errors (sb-posix:setpgid *shell-pgid* *shell-pgid*))
    (ignore-errors (tcsetpgrp *shell-terminal* *shell-pgid*))
    (setf *shell-tmodes* (sb-posix:tcgetattr *shell-terminal*))))

;;; --- Display strings ----------------------------------------------------

(defun command->string (cmd)
  (format nil "~{~A~^ ~}" (command-argv cmd)))

(defun pipeline->string (pl)
  (let ((s (format nil "~{~A~^ | ~}"
                   (mapcar #'command->string (pipeline-commands pl)))))
    (if (pipeline-background pl) (concatenate 'string s " &") s)))

;;; --- Redirections -------------------------------------------------------

(defun open-redir-file (type file)
  (ecase type
    (:in (sb-posix:open file sb-posix:o-rdonly))
    ;; A heredoc/here-string temp file: open for reading, then unlink it right
    ;; away -- the open fd keeps the data alive and no cleanup is needed.
    (:heredoc (let ((fd (sb-posix:open file sb-posix:o-rdonly)))
                (ignore-errors (delete-file file))
                fd))
    (:out (sb-posix:open file
                         (logior sb-posix:o-wronly sb-posix:o-creat sb-posix:o-trunc)
                         #o644))
    (:append (sb-posix:open file
                            (logior sb-posix:o-wronly sb-posix:o-creat sb-posix:o-append)
                            #o644))))

(defun apply-child-redir (r)
  "Apply redirection R permanently in the current (child) process."
  (destructuring-bind (type fd target) r
    (handler-case
        (if (eq type :dup)
            (sb-posix:dup2 target fd)
            (let ((newfd (open-redir-file type target)))
              (sb-posix:dup2 newfd fd)
              (unless (= newfd fd) (sb-posix:close newfd))))
      (error ()
        (format *error-output* "sbsh: ~A: cannot open~%" target)
        (finish-output *error-output*)
        (sb-ext:exit :code 1 :abort t)))))

(defun call-with-shell-redirections (redirs thunk)
  "Run THUNK with REDIRS applied to the shell's own fds, restoring after."
  (let ((saved '()))
    (unwind-protect
         (progn
           (dolist (r redirs)
             (destructuring-bind (type fd target) r
               (let ((save (sb-posix:dup fd)))
                 (push (cons fd save) saved)
                 (if (eq type :dup)
                     (sb-posix:dup2 target fd)
                     (let ((newfd (open-redir-file type target)))
                       (sb-posix:dup2 newfd fd)
                       (sb-posix:close newfd))))))
           (funcall thunk))
      (dolist (pair saved)
        (ignore-errors (sb-posix:dup2 (cdr pair) (car pair)))
        (ignore-errors (sb-posix:close (cdr pair)))))))

;;; --- Forking children ---------------------------------------------------

(defun child-setup (pgid infd outfd foreground fds-to-close redirs)
  "Post-fork setup in the child: join the process group, take the terminal if
foreground, restore default signal handling, wire up fds, and redirect."
  (let* ((pid (sb-posix:getpid))
         (group (if (zerop pgid) pid pgid)))
    (ignore-errors (sb-posix:setpgid pid group))
    (when (and foreground *interactive*)
      (ignore-errors (tcsetpgrp *shell-terminal* group)))
    (dolist (sig *jobctl-signals*)
      (ignore-errors (sb-sys:enable-interrupt sig :default)))
    (ignore-errors (sb-sys:enable-interrupt sb-posix:sigchld :default))
    (unless (= infd 0) (sb-posix:dup2 infd 0))
    (unless (= outfd 1) (sb-posix:dup2 outfd 1))
    (dolist (fd fds-to-close)
      (when (and fd (> fd 2)) (ignore-errors (sb-posix:close fd))))
    (dolist (r redirs) (apply-child-redir r))))

(defun fork-command (cmd pgid infd outfd foreground fds-to-close)
  "Fork and exec CMD.  Returns the child pid in the parent; never returns in
the child.  argv is built before the fork so the child does no allocation."
  ;; A Lisp filter stage: fork and run the form as a Lisp filter over stdin.
  (when (command-lisp cmd)
    (let ((pid (sb-posix:fork)))
      (if (not (zerop pid))
          (return-from fork-command pid)
          (progn
            (child-setup pgid infd outfd foreground fds-to-close nil)
            (run-lisp-filter (command-lisp cmd))
            (sb-ext:exit :code 0 :abort t)))))
  ;; A { } group or a shell-function call as a pipeline stage: fork and run it.
  (let ((fn (and (not (command-special cmd)) (command-argv cmd)
                 (shell-function (first (command-argv cmd))))))
    (when (or (command-special cmd) fn)
      (let ((pid (sb-posix:fork)))
        (if (not (zerop pid))
            (return-from fork-command pid)
            (progn
              (child-setup pgid infd outfd foreground fds-to-close
                           (command-redirs cmd))
              (let ((code (if (command-special cmd)
                              (progn (run-special (command-special cmd)) *last-status*)
                              (call-shell-function fn (rest (command-argv cmd))))))
                (finish-output)
                (sb-ext:exit :code (if (integerp code) code 0) :abort t)))))))
  (let* ((argv (command-argv cmd))
         (builtin (and argv (builtin-p (first argv))))
         (alien-argv (and argv (not builtin) (build-argv argv)))
         (path (and argv (first argv)))
         (pid (sb-posix:fork)))
    (cond
      ((not (zerop pid))
       (when alien-argv (sb-alien:free-alien alien-argv))
       pid)
      (t
       (child-setup pgid infd outfd foreground fds-to-close (command-redirs cmd))
       (cond
         ((null argv) (sb-ext:exit :code 0 :abort t))
         (builtin
          (let ((code (handler-case (run-builtin (first argv) (rest argv))
                        (error () 1))))
            (finish-output)
            (sb-ext:exit :code code :abort t)))
         (t
          (%execvp path alien-argv)
          (format *error-output* "sbsh: ~A: command not found~%" path)
          (finish-output *error-output*)
          (sb-ext:exit :code 127 :abort t)))))))

;;; --- Job foreground / background ----------------------------------------

(defun put-job-foreground (job cont)
  "Put JOB in the foreground and wait for it.  CONT resumes a stopped job."
  (setf (job-background job) nil)
  (when cont
    (format t "~A~%" (job-command job)))
  (when *interactive*
    (tcsetpgrp *shell-terminal* (job-pgid job)))
  (when cont
    (when *interactive*
      (ignore-errors (restore-mode (job-tmodes job) *shell-terminal*)))
    (mark-job-running job)
    (sb-posix:killpg (job-pgid job) sb-posix:sigcont))
  (wait-for-job job)
  (when *interactive*
    ;; Save the job's terminal modes and restore the shell's.
    (setf (job-tmodes job) (ignore-errors (sb-posix:tcgetattr *shell-terminal*)))
    (tcsetpgrp *shell-terminal* *shell-pgid*)
    (restore-mode *shell-tmodes* *shell-terminal*))
  (post-foreground-cleanup job))

(defun put-job-background (job cont)
  "Put JOB in the background; CONT resumes a stopped job."
  (setf (job-background job) t)
  (when cont
    (mark-job-running job)
    (sb-posix:killpg (job-pgid job) sb-posix:sigcont)
    (format t "[~D]+ ~A &~%" (job-id job) (job-command job))))

(defun post-foreground-cleanup (job)
  (cond
    ((job-completed-p job)
     (setf *last-status* (job-exit-code job))
     (remove-job job))
    ((job-stopped-p job)
     (setf (job-background job) t
           (job-notified job) t)   ; we print it here; don't re-notify
     (format t "~%[~D]+  Stopped~vT~A~%" (job-id job) 14 (job-command job))
     (setf *last-status* 148))))  ; 128 + SIGTSTP

;;; --- Launching a pipeline -----------------------------------------------

(defun standalone-builtin-p (pipeline)
  "True when PIPELINE is a single foreground builtin we can run in-process."
  (let ((cmds (pipeline-commands pipeline)))
    (and (= (length cmds) 1)
         (not (pipeline-background pipeline))
         (not (command-lisp (first cmds)))
         (let ((argv (command-argv (first cmds))))
           (or (null argv) (builtin-p (first argv)))))))

(defun run-standalone-builtin (pipeline)
  (let* ((cmd (first (pipeline-commands pipeline)))
         (argv (command-argv cmd)))
    (flet ((run () (if argv
                       (run-builtin (first argv) (rest argv))
                       ;; A pure assignment (empty argv) takes the status of the
                       ;; last command substitution in its value, else 0.
                       (or *cmdsub-status* 0))))
      (setf *last-status*
            (if (command-redirs cmd)
                (handler-case
                    (call-with-shell-redirections (command-redirs cmd) #'run)
                  (sb-posix:syscall-error (e)
                    (format *error-output* "sbsh: redirection: ~A~%" e)
                    1))
                (run))))
    *last-status*))

(defun assignment-word-p (s)
  "True if S looks like NAME=VALUE with a valid identifier NAME."
  (let ((eq (position #\= s)))
    (and eq (> eq 0)
         (let ((c0 (char s 0)))
           (and (or (alpha-char-p c0) (char= c0 #\_))
                (loop for i from 1 below eq
                      always (let ((c (char s i)))
                               (or (alphanumericp c) (char= c #\_)))))))))

(defun expand-assignment-value (v)
  "Expand a leading ~ in each :-separated segment of an assignment value
(so PATH=~/bin:~/x and x=~/foo work)."
  (with-output-to-string (out)
    (loop for seg in (split-on-char v #\:)
          for first = t then nil
          do (unless first (write-char #\: out))
             (write-string (if (and (plusp (length seg)) (char= (char seg 0) #\~))
                               (expand-tilde seg) seg)
                           out))))

(defun apply-assignment (s)
  (let ((eq (position #\= s)))
    (sb-posix:setenv (subseq s 0 eq) (expand-assignment-value (subseq s (1+ eq))) 1)))

(defun strip-leading-assignments (cmd)
  "Move any leading NAME=VALUE words of CMD into the environment, updating the
command's ARGV to the remaining words.  Returns nothing."
  (let* ((argv (command-argv cmd))
         (i (loop for a in argv while (assignment-word-p a) count t)))
    (when (plusp i)
      (dolist (a (subseq argv 0 i)) (apply-assignment a))
      (setf (command-argv cmd) (subseq argv i)))))

;;; --- Shell functions and { } groups -------------------------------------

(defstruct shfun name body source)

(defun shell-function (name)
  (gethash name *functions*))

(defun run-special (special)
  "Run a special command: define a function or execute a { } group."
  (destructuring-bind (kind . rest) special
    (ecase kind
      (:defun
       (destructuring-bind (name body) rest
         (setf (gethash name *functions*)
               (make-shfun :name name :body body :source body))
         (setf *last-status* 0)))
      (:group
       (run-command-line (first rest)))
      (:compound
       (eval-compound (first rest))))))

(defun call-shell-function (fn args)
  "Invoke shell function FN with ARGS bound as positional parameters.  A
`return` unwinds via the SBSH-RETURN catch; `local`s are restored on exit."
  (let ((*positional* args)
        (*in-function* t)
        (*heredoc-bodies* nil)
        (*function-local-restores* '()))
    (unwind-protect
         (catch 'sbsh-return
           (run-command-line (shfun-body fn))
           *last-status*)
      (dolist (r *function-local-restores*) (ignore-errors (funcall r))))))

(defun launch-pipeline (pipeline)
  "Run PIPELINE, then apply $PIPESTATUS, pipefail, and ! negation."
  (setf *pipestatus* nil)
  (%launch-pipeline pipeline)
  (unless *pipestatus* (setf *pipestatus* (list *last-status*)))
  (when *pipefail*
    (setf *last-status*
          (or (find-if (lambda (s) (not (eql s 0))) (reverse *pipestatus*)) 0)))
  (when (pipeline-negate pipeline)
    (setf *last-status* (if (zerop *last-status*) 1 0)))
  *last-status*)

(defun %launch-pipeline (pipeline)
  "Fork the commands of PIPELINE into a process group and run it."
  (mapc #'realize-command (pipeline-commands pipeline))
  (dolist (c (pipeline-commands pipeline))
    (push (if (command-lisp c) (list :lisp) (command-argv c)) *line-commands*))
  (let ((cmds (pipeline-commands pipeline)))
    ;; Single-command pipelines that must run in the shell process itself
    ;; (so they affect the live image / shell state): specials, Lisp, functions.
    (when (and (= (length cmds) 1) (not (pipeline-background pipeline)))
      (let* ((cmd (first cmds)))
        (when (command-special cmd)
          (return-from %launch-pipeline
            (if (command-redirs cmd)
                (call-with-shell-redirections
                 (command-redirs cmd)
                 (lambda () (run-special (command-special cmd))))
                (run-special (command-special cmd)))))
        (when (command-lisp cmd)
          (return-from %launch-pipeline (run-lisp-in-process (command-lisp cmd))))
        (strip-leading-assignments cmd)
        (let ((fn (and (command-argv cmd) (shell-function (first (command-argv cmd))))))
          (when fn
            (return-from %launch-pipeline
              (setf *last-status*
                    (call-shell-function fn (rest (command-argv cmd)))))))))
    ;; Leading VAR=value assignments on the first command set the environment.
    (when (and (first cmds) (not (command-special (first cmds))))
      (strip-leading-assignments (first cmds))))
  (when (standalone-builtin-p pipeline)
    (return-from %launch-pipeline (run-standalone-builtin pipeline)))
  (preflight-commands pipeline)
  (let* ((cmds (pipeline-commands pipeline))
         (bg (pipeline-background pipeline))
         (foreground (not bg))
         (job (make-job :id (next-job-id)
                        :command (pipeline->string pipeline)
                        :background bg))
         (pgid 0)
         (prev-read nil)
         (procs '()))
    (loop for rest on cmds
          for cmd = (car rest)
          do (let (pipe-read pipe-write)
               (when (cdr rest)
                 (multiple-value-setq (pipe-read pipe-write) (sb-posix:pipe)))
               (let* ((infd (or prev-read 0))
                      (outfd (or pipe-write 1))
                      (pid (fork-command cmd pgid infd outfd foreground
                                         (list prev-read pipe-read pipe-write))))
                 (when (zerop pgid) (setf pgid pid))
                 (ignore-errors (sb-posix:setpgid pid pgid))
                 (push (make-proc :pid pid :command (command->string cmd)) procs))
               (when prev-read (sb-posix:close prev-read))
               (when pipe-write (sb-posix:close pipe-write))
               (setf prev-read pipe-read)))
    (setf (job-pgid job) pgid
          (job-procs job) (nreverse procs))
    (add-job job)
    (if bg
        (progn
          (when *interactive*
            (format *error-output* "[~D] ~D~%" (job-id job) pgid))
          (setf *last-status* 0))
        (progn
          (put-job-foreground job nil)
          (setf *pipestatus* (mapcar #'proc-exit-code (job-procs job)))))
    *last-status*))

;;; --- And-or list evaluation ---------------------------------------------

(defun run-command-line (string)
  "Parse and execute a full command line, honoring && || ; & connectors.
Each clause is tokenized/parsed lazily, right before it runs, so expansions
reflect state produced by earlier clauses on the same line."
  (let ((run-next t) (clauses (split-clauses string)))
    (loop for (cl . rest) on clauses
          for term = (getf cl :terminator)
          ;; A clause that feeds a && / || (or is a && / || operand) is a
          ;; condition: its failure is expected, so errexit is suppressed.
          for cond-p = (or *condition-context* (member term '(:and :or)))
          do (when run-next
               (let ((*condition-context* cond-p))
                 (run-clause (getf cl :text) term)))
             ;; errexit: exit if a plain statement failed.
             (when (and *errexit* run-next (not cond-p) (not *should-exit*)
                        (not (zerop *last-status*)))
               (setf *should-exit* *last-status*))
             (setf run-next
                   (case term
                     (:and (zerop *last-status*))
                     (:or (not (zerop *last-status*)))
                     (t t)))
             (when *should-exit* (return))))
  *last-status*)

(defun run-clause (text term)
  "Parse and run one clause.  Errors are confined to this clause so that
`;`-separated clauses after a failure still run.  An unknown command triggers
the interactive correction menu (innermost handler) and otherwise fails 127."
  (setf *cmdsub-status* nil)   ; reset before parsing (where $(...) runs)
  (handler-case
      (handler-bind
          ((command-not-found
             (lambda (c)
               (when (and *interactive* (not *capturing*))
                 (let ((choice (offer-correction c)))
                   (when choice (invoke-restart 'use-command choice)))))))
        (let ((pl (parse-segment text)))
          (when pl
            (when (eq term :amp) (setf (pipeline-background pl) t))
            (launch-pipeline pl))))
    (command-not-found (c)
      (unless *interactive*
        (format *error-output* "sbsh: ~A: command not found~%"
                (command-not-found-name c)))
      (setf *last-status* 127))
    (shell-parse-error (e)
      (format *error-output* "sbsh: syntax error: ~A~%" (parse-error-message e))
      (setf *last-status* 2))
    (sb-posix:syscall-error (e)
      (format *error-output* "sbsh: ~A~%" e)
      (setf *last-status* 1))
    (shell-error (e)
      (format *error-output* "sbsh: ~A~%" e)
      (setf *last-status* 1))))

;;; --- Top-level entry ----------------------------------------------------

(defun execute-line (line &optional bodies)
  "Parse and execute a full command line, recording a history entry.  BODIES is
the ordered list of heredoc bodies the reader collected for this line.  Errors
are handled per clause in RUN-CLAUSE; this is just a last-resort guard."
  (let ((*line-commands* '())
        (*heredoc-bodies* bodies)
        (*heredoc-temps* '()))
    (unwind-protect
         (prog1
             (handler-case (run-command-line line)
               (shell-error (c)
                 (format *error-output* "sbsh: ~A~%" c)
                 (setf *last-status* 1)))
           (record-history-line line))
      ;; Backstop: remove any heredoc temp files not already unlinked on open.
      (dolist (p *heredoc-temps*) (ignore-errors (delete-file p))))))

;;; --- Command resolution and "did you mean?" suggestions -----------------

(defun resolve-executable (name)
  "Return the resolved path of external command NAME, or NIL if not found."
  (path-search name))

(defun suggest-commands (name)
  "Return up to five builtin/PATH command names within edit distance 2 of NAME."
  (let ((cands '()))
    (maphash (lambda (k v) (declare (ignore v)) (push k cands)) *builtins*)
    (dolist (dir (split-on-char (or (getenv "PATH") "") #\:))
      (when (plusp (length dir))
        (dolist (p (ignore-errors
                    (uiop:directory-files
                     (concatenate 'string (string-right-trim "/" dir) "/"))))
          (let ((n (file-namestring p)))
            (when (plusp (length n)) (push n cands))))))
    (let ((scored (loop for c in (remove-duplicates cands :test #'string=)
                        for d = (levenshtein name c 2)
                        when (<= d 2) collect (cons c d))))
      (setf scored (sort scored #'< :key #'cdr))
      (mapcar #'car (subseq scored 0 (min 5 (length scored)))))))

(defun preflight-commands (pipeline)
  "Before forking, verify each external stage resolves on $PATH.  Signals a
correctable COMMAND-NOT-FOUND (with a USE-COMMAND restart) otherwise."
  (dolist (cmd (pipeline-commands pipeline))
    (let ((argv (command-argv cmd)))
      (when (and argv
                 (not (command-lisp cmd))
                 (not (command-special cmd))
                 (not (builtin-p (first argv)))
                 (not (shell-function (first argv)))
                 (not (resolve-executable (first argv))))
        (restart-case
            (error 'command-not-found
                   :name (first argv)
                   :suggestions (suggest-commands (first argv)))
          (use-command (new-name)
            :report "Run a different command instead"
            (setf (command-argv cmd) (cons new-name (rest argv)))))))))

(defun offer-correction (c)
  "Interactively present suggestions for a COMMAND-NOT-FOUND condition C.
Returns the chosen command string, or NIL to give up."
  (let ((sugg (command-not-found-suggestions c)))
    (format t "sbsh: ~A: command not found~%" (command-not-found-name c))
    (when sugg
      (format t "Did you mean:~%")
      (loop for s in sugg for i from 1 do (format t "  [~D] ~A~%" i s))
      (format t "Run which? [1-~D, Enter to cancel] " (length sugg))
      (force-output)
      (let* ((line (read-line (tty-in) nil ""))
             (n (parse-integer line :junk-allowed t)))
        (when (and n (<= 1 n (length sugg)))
          (nth (1- n) sugg))))))

;;; --- Lisp evaluation and pipeline stages --------------------------------

(defun print-lisp-value (value)
  "Print the value of an interactive `(...)` Lisp escape, REPL-style."
  (unless (null value)
    (fresh-line)
    (prin1 value)
    (terpri)
    (force-output)))

(defun run-lisp-in-process (form)
  "Evaluate FORM in the shell process (so definitions mutate the live image),
print its value, and set the exit status."
  (handler-case
      (let* ((*package* *user-package*)
             (value (progv (list 'sbsh-user::lines 'sbsh-user::input)
                        (list nil "")
                      (eval form))))
        (print-lisp-value value)
        (setf *last-status* 0))
    (error (e)
      (format *error-output* "sbsh: lisp: ~A~%" e)
      (setf *last-status* 1)))
  *last-status*)

(defun emit-lisp-result (value out)
  "Write VALUE to OUT as text: a list becomes one line per element."
  (cond
    ((null value))
    ((stringp value)
     (write-string value out)
     (unless (and (plusp (length value))
                  (char= (char value (1- (length value))) #\Newline))
       (terpri out)))
    ((and (listp value) (not (null value)))
     (dolist (x value) (princ x out) (terpri out)))
    (t (princ value out) (terpri out))))

(defun run-lisp-filter (form)
  "In a forked child: read stdin into `lines`/`input`, evaluate FORM, and
write the result to stdout."
  (handler-case
      (let* ((in (sb-sys:make-fd-stream 0 :input t :external-format :utf-8))
             (out (sb-sys:make-fd-stream 1 :output t :external-format :utf-8))
             (all (loop for l = (read-line in nil nil) while l collect l))
             (*package* *user-package*)
             (value (progv (list 'sbsh-user::lines 'sbsh-user::input)
                        (list all (format nil "~{~A~%~}" all))
                      (eval form))))
        (emit-lisp-result value out)
        (finish-output out))
    (error (e)
      (ignore-errors (format *error-output* "sbsh: lisp: ~A~%" e)))))

;;; --- Command substitution $(...) ----------------------------------------

(defvar *tmp-counter* 0)

(defun temp-file ()
  (format nil "/tmp/sbsh-sub-~D-~D" (sb-posix:getpid) (incf *tmp-counter*)))

(defun write-heredoc-temp (content)
  "Write CONTENT to a fresh temp file and return its path."
  (let ((path (temp-file)))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create :external-format :utf-8)
      (write-string content out))
    path))

(defun command-substitute (body)
  "Value of a $(...) substitution.  A leading ( is evaluated as Lisp (with
shell $VAR/$1/$(...) references in the form expanded first, so arithmetic like
$((- $1 1)) works); anything else is run as a shell command whose stdout is
captured."
  (if (lisp-stage-p body)
      (handler-case
          (let ((*package* *user-package*))
            (princ-to-string (eval (read-from-string (expand-heredoc-body body)))))
        (error () ""))
      (capture-command-output body)))

(defun capture-command-output (line)
  "Run LINE with stdout redirected to a temp file and return its contents,
with trailing newlines stripped.  Deadlock-free (uses a file, not a pipe)."
  (let ((tmp (temp-file)))
    (unwind-protect
         (let ((*capturing* t))
           (finish-output *standard-output*)
           (call-with-shell-redirections
            (list (list :out 1 tmp))
            (lambda ()
              (execute-line line)
              (finish-output *standard-output*)))
           (setf *cmdsub-status* *last-status*)   ; x=$(false) -> $? = 1
           (string-right-trim '(#\Newline #\Return)
                              (if (probe-file tmp)
                                  (read-file-into-string tmp)
                                  "")))
      (ignore-errors (delete-file tmp)))))
