;;;; repl.lisp --- The interactive read-eval-print loop.

(in-package #:sbsh)

(defun format-prompt ()
  "Build the interactive prompt: user@host:cwd$ with a color when a TTY."
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
  "Current directory with $HOME abbreviated to ~."
  (let ((cwd (or (ignore-errors (sb-posix:getcwd)) "?"))
        (home (getenv "HOME")))
    (if (and home (starts-with-subseq home cwd))
        (concatenate 'string "~" (subseq cwd (length home)))
        cwd)))

(defun read-command ()
  "Read one command line, using the interactive editor when on a TTY.
Returns a string, :EOF, or :CANCEL."
  (if *interactive*
      (read-line-interactive (format-prompt))
      (progn
        (format t "~A" (format-prompt))
        (force-output)
        (let ((line (read-line *standard-input* nil :eof)))
          line))))

(defun run-shell ()
  "Main interactive loop.  Returns the shell's exit code."
  (init-job-control)
  (ignore-errors (load-history))
  (loop
    (when *should-exit* (return *should-exit*))
    ;; Report background jobs that changed state.
    (reap-children)
    (notify-finished-jobs)
    (let ((line (read-command)))
      (cond
        ((eq line :eof)
         (when *interactive* (format t "exit~%"))
         (return (or *should-exit* *last-status*)))
        ((eq line :cancel)
         (setf *last-status* 130))         ; 128 + SIGINT
        ((and (stringp line) (plusp (length (string-trim '(#\Space #\Tab) line))))
         (history-add line)
         (run-command-line line))
        (t nil)))))
