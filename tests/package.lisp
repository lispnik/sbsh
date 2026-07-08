;;;; tests/package.lisp

(defpackage #:sbsh/tests
  (:use #:cl #:fiveam)
  (:export #:all-tests #:run-tests))

(in-package #:sbsh/tests)

(def-suite all-tests :description "All sbsh tests.")
(in-suite all-tests)

(defun run-tests ()
  (run! 'all-tests))
