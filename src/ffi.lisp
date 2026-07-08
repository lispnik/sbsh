;;;; ffi.lisp --- Foreign functions SBCL's sb-posix does not export.
;;;;
;;;; sb-posix already provides fork, waitpid, dup2, pipe, setpgid, killpg,
;;;; termios, and the signal constants.  These four calls are the gaps we
;;;; need for real job control and a raw-mode line editor.

(in-package #:sbsh)

(sb-alien:define-alien-routine ("execvp" %execvp) sb-alien:int
  (file sb-alien:c-string)
  (argv (sb-alien:* sb-alien:c-string)))

(sb-alien:define-alien-routine ("tcsetpgrp" %tcsetpgrp) sb-alien:int
  (fd sb-alien:int)
  (pgrp sb-alien:int))

(sb-alien:define-alien-routine ("tcgetpgrp" %tcgetpgrp) sb-alien:int
  (fd sb-alien:int))

(sb-alien:define-alien-routine ("isatty" %isatty) sb-alien:int
  (fd sb-alien:int))

(defun tty-p (fd)
  "Return true if FD refers to a terminal."
  (= 1 (%isatty fd)))

(defun tcsetpgrp (fd pgrp)
  "Set the foreground process group of the terminal FD to PGRP."
  (%tcsetpgrp fd pgrp))

(defun tcgetpgrp (fd)
  "Return the foreground process group of the terminal FD."
  (%tcgetpgrp fd))

(defun build-argv (args)
  "Allocate a NULL-terminated C string array from the list of strings ARGS.
The caller is responsible for FREE-ALIEN once the child has exec'd or failed.
We build this in the parent before forking so the child does no allocation."
  (let* ((n (length args))
         (argv (sb-alien:make-alien sb-alien:c-string (1+ n))))
    (loop for i from 0
          for a in args
          do (setf (sb-alien:deref argv i) a))
    (setf (sb-alien:deref argv n) nil)
    argv))

(defun exec-program (path args)
  "Replace the current process image with PATH, passing ARGS as argv.
Only returns (with NIL) if the exec fails."
  (let ((argv (build-argv args)))
    (%execvp path argv)
    ;; execvp only returns on error.
    (sb-alien:free-alien argv)
    nil))
