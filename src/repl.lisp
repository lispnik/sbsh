;;;; repl.lisp --- The interactive read-eval-print loop.

(in-package #:sbsh)

(defun format-prompt ()
  "Build the interactive prompt.  Uses the user's *PROMPT-FN* when set."
  (or (when *prompt-fn*
        (handler-case (let ((*package* *user-package*)) (funcall *prompt-fn*))
          (error () nil)))
      (default-prompt)))

(defun default-prompt ()
  "The built-in prompt: user@host:cwd$ with color on a TTY."
  (let* ((user (or (getenv "USER") "?"))
         (host (short-hostname))
         (cwd (pretty-cwd))
         (mark (if (zerop (sb-posix:getuid)) "#" "$")))
    (if *interactive*
        (format nil "~C[1;32m~A@~A~C[0m:~C[1;34m~A~C[0m~A "
                #\Escape user host #\Escape #\Escape cwd #\Escape mark)
        (format nil "~A@~A:~A~A " user host cwd mark))))

(defun short-hostname ()
  (let ((name (or (ignore-errors (machine-instance)) "localhost")))
    (subseq name 0 (or (position #\. name) (length name)))))

(defun pretty-cwd ()
  "Current directory (logical $PWD) with $HOME abbreviated to ~."
  (let ((cwd (or (getenv "PWD") (ignore-errors (sb-posix:getcwd)) "?"))
        (home (getenv "HOME")))
    (if (and home (starts-with-subseq home cwd))
        (concatenate 'string "~" (subseq cwd (length home)))
        cwd)))

(defun read-raw-line (prompt)
  "Read a single physical line with PROMPT (the editor on a TTY, otherwise a
plain read-line).  Returns a string, :EOF, or :CANCEL."
  (if *interactive*
      (read-line-interactive prompt)
      (progn
        (format t "~A" prompt)
        (force-output)
        (read-line *standard-input* nil :eof))))

(defun continuation-prompt (reason)
  "Secondary prompt shown while gathering a continued command."
  (if *interactive*
      (case reason
        (:quote "quote> ")
        (:paren "paren> ")
        (t "> "))
      ""))

(defun read-command ()
  "Read one logical command, continuing across physical lines while incomplete
(open quote/paren, trailing backslash, dangling operator) and gathering any
heredoc bodies.  Returns a LOGICAL-LINE, :EOF, or :CANCEL."
  (read-logical-command
   (lambda (context)
     (read-raw-line
      (cond ((eq context :first) (format-prompt))
            ((stringp context) "> ")               ; a heredoc body line
            (t (continuation-prompt context)))))))

(defun run-shell ()
  "Main interactive loop.  Returns the shell's exit code."
  (init-job-control)
  (ignore-errors (load-history))
  (ignore-errors (load-records))
  (ignore-errors (load-rc-file))
  (loop
    (when *should-exit* (return *should-exit*))
    ;; Report background jobs that changed state.
    (reap-children)
    (notify-finished-jobs)
    (let ((cmd (read-command)))
      (cond
        ((eq cmd :eof)
         (when *interactive* (format t "exit~%"))
         (return (or *should-exit* *last-status*)))
        ((eq cmd :cancel)
         (setf *last-status* 130))         ; 128 + SIGINT
        ((logical-line-p cmd)
         (let ((text (logical-line-text cmd)))
           (when (plusp (length (string-trim '(#\Space #\Tab #\Newline) text)))
             (history-add text)
             (execute-line text (logical-line-bodies cmd)))))
        (t nil)))))
