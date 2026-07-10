;;;; main.lisp --- Command-line entry point.

(in-package #:sbsh)

(defun print-usage ()
  (format t "Usage: sbsh [OPTIONS] [FILE]~%~%")
  (format t "  -c COMMAND   Execute COMMAND and exit~%")
  (format t "  -h, --help   Show this help~%")
  (format t "  -v, --version Show version~%")
  (format t "  FILE         Read and execute commands from FILE~%"))

(defun run-lines (next-line-fn)
  "Read and execute logical commands (joining continuations and heredoc bodies)
using NEXT-LINE-FN, a function returning the next physical line or :EOF."
  (loop
    (let ((cmd (read-logical-command
                (lambda (context) (declare (ignore context)) (funcall next-line-fn)))))
      (when (or (eq cmd :eof) *should-exit*) (return))
      (when (logical-line-p cmd)
        (execute-line (logical-line-text cmd) (logical-line-bodies cmd))))))

(defun run-script-file (path)
  "Execute the script at PATH non-interactively, joining continued lines and
gathering heredoc bodies."
  (setf *interactive* nil)
  (with-open-file (in path :external-format :utf-8)
    (run-lines (lambda () (read-line in nil :eof))))
  (or *should-exit* *last-status*))

(defun run-command-string (string)
  "Execute STRING (the -c argument) as a mini-script so heredocs work."
  (let ((lines (split-on-char string #\Newline)))
    (run-lines (lambda () (if lines (pop lines) :eof)))))

(defun main ()
  "Program entry point; dispatches between -c, script, and interactive use."
  (let ((args (rest sb-ext:*posix-argv*)))
    (handler-case
        (cond
          ((null args)
           (sb-ext:exit :code (run-shell)))
          ((member (first args) '("-h" "--help") :test #'string=)
           (print-usage) (sb-ext:exit :code 0))
          ((member (first args) '("-v" "--version") :test #'string=)
           (format t "sbsh ~A~%" (or (asdf-version) "0.1.0"))
           (sb-ext:exit :code 0))
          ((string= (first args) "-c")
           (setf *interactive* nil)
           (when (second args) (run-command-string (second args)))
           (sb-ext:exit :code (or *should-exit* *last-status*)))
          (t
           (sb-ext:exit :code (run-script-file (first args)))))
      (sb-sys:interactive-interrupt ()
        (sb-ext:exit :code 130))
      #+sbcl
      (serious-condition (e)
        (format *error-output* "sbsh: fatal: ~A~%" e)
        (sb-ext:exit :code 1)))))

(defun asdf-version ()
  (ignore-errors
    (asdf:component-version (asdf:find-system :sbsh))))
