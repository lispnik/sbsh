;;;; util.lisp --- Tiny self-contained utilities.
;;;;
;;;; sbsh depends only on SBCL itself (sb-posix / sb-alien).  These two
;;;; helpers are all we borrowed from a utility library, so we keep them
;;;; here rather than take a runtime dependency.

(in-package #:sbsh)

(defun starts-with-subseq (prefix string)
  "True if STRING begins with PREFIX (both strings)."
  (let ((pl (length prefix)))
    (and (<= pl (length string))
         (string= prefix string :end2 pl))))

(defun read-file-into-string (path)
  "Read the entire file at PATH into a string (UTF-8)."
  (with-open-file (in path :element-type 'character :external-format :utf-8)
    (let ((buffer (make-string (file-length in))))
      (subseq buffer 0 (read-sequence buffer in)))))
