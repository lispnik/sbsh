;;;; sbsh.asd --- A Unix shell written in Common Lisp on SBCL.
;;;;
;;;; SPDX-License-Identifier: MIT

(asdf:defsystem #:sbsh
  :description "sbsh: a Unix shell in Common Lisp with pipelines, job control, and history."
  :author "burnsidemk@gmail.com"
  :license "MIT"
  :version "0.1.0"
  :depends-on ((:require #:sb-posix))
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "util")
                             (:file "conditions")
                             (:file "ffi")
                             (:file "terminal")
                             (:file "history")
                             (:file "line-editor")
                             (:file "lexer")
                             (:file "parser")
                             (:file "jobs")
                             (:file "builtins")
                             (:file "exec")
                             (:file "config")
                             (:file "repl")
                             (:file "main"))))
  :build-operation "program-op"
  :build-pathname "sbsh"
  :entry-point "sbsh:main"
  :in-order-to ((asdf:test-op (asdf:test-op #:sbsh/tests))))

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

(asdf:defsystem #:sbsh/tests
  :description "Test suite for sbsh."
  :author "burnsidemk@gmail.com"
  :license "MIT"
  :depends-on (#:sbsh #:fiveam)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "suite"))))
  :perform (asdf:test-op (o c)
             (uiop:symbol-call :fiveam :run! (uiop:find-symbol* :all-tests :sbsh/tests))))
