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

;;; --- posix_spawn -------------------------------------------------------
;;; Launching an external command with posix_spawn(3) avoids fork()ing the whole
;;; SBCL image -- ~250x faster on macOS (no VM-map copy / COW faults) and ~6x on
;;; Linux.  The opaque posix_spawn_file_actions_t / posix_spawnattr_t / sigset_t
;;; are passed as caller-allocated blocks, sized generously for any platform
;;; (glibc's posix_spawnattr_t alone is ~340 bytes).

(sb-alien:define-alien-routine ("posix_spawnp" %posix-spawnp) sb-alien:int
  (pid (sb-alien:* sb-alien:int))
  (file sb-alien:c-string)
  (file-actions sb-alien:system-area-pointer)
  (attrp sb-alien:system-area-pointer)
  (argv (sb-alien:* sb-alien:c-string))
  (envp sb-alien:system-area-pointer))

(macrolet ((def (c lisp &rest extra)
             `(sb-alien:define-alien-routine (,c ,lisp) sb-alien:int
                (obj sb-alien:system-area-pointer) ,@extra)))
  (def "posix_spawn_file_actions_init"    %fa-init)
  (def "posix_spawn_file_actions_destroy" %fa-destroy)
  (def "posix_spawn_file_actions_adddup2" %fa-adddup2 (fd sb-alien:int) (newfd sb-alien:int))
  (def "posix_spawn_file_actions_addclose" %fa-addclose (fd sb-alien:int))
  (def "posix_spawn_file_actions_addopen" %fa-addopen
       (fd sb-alien:int) (path sb-alien:c-string) (oflag sb-alien:int) (mode sb-alien:int))
  (def "posix_spawnattr_init"    %attr-init)
  (def "posix_spawnattr_destroy" %attr-destroy)
  (def "posix_spawnattr_setflags" %attr-setflags (flags sb-alien:short))
  (def "posix_spawnattr_setpgroup" %attr-setpgroup (pg sb-alien:int))
  (def "posix_spawnattr_setsigdefault" %attr-setsigdefault (set sb-alien:system-area-pointer))
  (def "sigemptyset" %sigemptyset)
  (def "sigaddset" %sigaddset (sig sb-alien:int)))

(defconstant +posix-spawn-setpgroup+ #x0002)
(defconstant +posix-spawn-setsigdef+ #x0004)
(defconstant +spawn-opaque-size+ 1024
  "Bytes reserved for each opaque posix_spawn struct (max over all platforms).")

(defun make-opaque ()
  "Allocate a zeroed, pointer-aligned block big enough for any of the opaque
posix_spawn structs."
  (let ((a (sb-alien:make-alien (sb-alien:unsigned 64)
                                (truncate +spawn-opaque-size+ 8))))
    (dotimes (i (truncate +spawn-opaque-size+ 8)) (setf (sb-alien:deref a i) 0))
    a))

(defun environ-sap ()
  "SAP of the current environment (char **environ) for posix_spawn's envp."
  (sb-alien:alien-sap
   (sb-alien:extern-alien "environ" (sb-alien:* (sb-alien:* sb-alien:char)))))
