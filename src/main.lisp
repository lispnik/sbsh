;;;; main.lisp --- Command-line entry point.

(in-package #:sbsh)

(defun print-usage ()
  (format t "Usage: sbsh [OPTIONS] [FILE]~%~%")
  (format t "  -c COMMAND   Execute COMMAND and exit~%")
  (format t "  -h, --help   Show this help~%")
  (format t "  -v, --version Show version~%")
  (format t "  FILE         Read and execute commands from FILE~%"))

(defun run-script-file (path)
  "Execute each line of the script at PATH non-interactively."
  (setf *interactive* nil)
  (with-open-file (in path :external-format :utf-8)
    (loop for line = (read-line in nil :eof)
          until (or (eq line :eof) *should-exit*)
          do (run-command-line line)))
  (or *should-exit* *last-status*))

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
           (when (second args) (run-command-line (second args)))
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
