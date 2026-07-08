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
         (let ((argv (command-argv (first cmds))))
           (or (null argv) (builtin-p (first argv)))))))

(defun run-standalone-builtin (pipeline)
  (let* ((cmd (first (pipeline-commands pipeline)))
         (argv (command-argv cmd)))
    (flet ((run () (if argv (run-builtin (first argv) (rest argv)) 0)))
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

(defun apply-assignment (s)
  (let ((eq (position #\= s)))
    (sb-posix:setenv (subseq s 0 eq) (subseq s (1+ eq)) 1)))

(defun strip-leading-assignments (cmd)
  "Move any leading NAME=VALUE words of CMD into the environment, updating the
command's ARGV to the remaining words.  Returns nothing."
  (let* ((argv (command-argv cmd))
         (i (loop for a in argv while (assignment-word-p a) count t)))
    (when (plusp i)
      (dolist (a (subseq argv 0 i)) (apply-assignment a))
      (setf (command-argv cmd) (subseq argv i)))))

(defun launch-pipeline (pipeline)
  "Fork the commands of PIPELINE into a process group and run it."
  (mapc #'realize-command (pipeline-commands pipeline))
  ;; Leading VAR=value assignments on the first command set the environment.
  (when (pipeline-commands pipeline)
    (strip-leading-assignments (first (pipeline-commands pipeline))))
  (when (standalone-builtin-p pipeline)
    (return-from launch-pipeline (run-standalone-builtin pipeline)))
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
          (format t "[~D] ~D~%" (job-id job) pgid)
          (setf *last-status* 0))
        (put-job-foreground job nil))
    *last-status*))

;;; --- And-or list evaluation ---------------------------------------------

(defun run-command-line (string)
  "Parse and execute a full command line, honoring && || ; & connectors.
Each clause is tokenized/parsed lazily, right before it runs, so expansions
reflect state produced by earlier clauses on the same line."
  (let ((run-next t))
    (dolist (cl (split-clauses string))
      (let ((term (getf cl :terminator)))
        (when run-next
          (handler-case
              (let ((pl (parse-segment (getf cl :text))))
                (when pl
                  (when (eq term :amp) (setf (pipeline-background pl) t))
                  (launch-pipeline pl)))
            (shell-parse-error (e)
              (format *error-output* "sbsh: syntax error: ~A~%"
                      (parse-error-message e))
              (setf *last-status* 2))
            (sb-posix:syscall-error (e)
              (format *error-output* "sbsh: ~A~%" e)
              (setf *last-status* 1))))
        (setf run-next
              (case term
                (:and (zerop *last-status*))
                (:or (not (zerop *last-status*)))
                (t t))))))
  *last-status*)
