;;;; tests/suite.lisp --- Unit tests for the pure parts of sbsh.

(in-package #:sbsh/tests)
(in-suite all-tests)

;;; --- Globbing / fnmatch -------------------------------------------------

(test fnmatch-star
  (is-true  (sbsh::fnmatch "*.lisp" "foo.lisp"))
  (is-true  (sbsh::fnmatch "*.lisp" ".lisp"))
  (is-false (sbsh::fnmatch "*.lisp" "foo.txt"))
  (is-true  (sbsh::fnmatch "*" "anything"))
  (is-true  (sbsh::fnmatch "a*b*c" "axxbyyc")))

(test fnmatch-question
  (is-true  (sbsh::fnmatch "?at" "cat"))
  (is-false (sbsh::fnmatch "?at" "at"))
  (is-false (sbsh::fnmatch "?at" "chat")))

(test fnmatch-class
  (is-true  (sbsh::fnmatch "[abc]at" "bat"))
  (is-false (sbsh::fnmatch "[abc]at" "dat"))
  (is-true  (sbsh::fnmatch "[a-z]" "m"))
  (is-false (sbsh::fnmatch "[a-z]" "M"))
  (is-true  (sbsh::fnmatch "[!0-9]" "x"))
  (is-false (sbsh::fnmatch "[!0-9]" "5")))

;;; --- Tokenizer ----------------------------------------------------------

(defun word-texts (tokens)
  (loop for tok in tokens when (sbsh::word-p tok) collect (sbsh::word-text tok)))

(test tokenize-basic
  (is (equal '("ls" "-la" "/tmp")
             (word-texts (sbsh::tokenize "ls -la /tmp")))))

(test tokenize-quotes
  (is (equal '("hello world")
             (word-texts (sbsh::tokenize "\"hello world\""))))
  (is (equal '("a b" "c")
             (word-texts (sbsh::tokenize "'a b' c"))))
  (is (equal '("a$b")
             (word-texts (sbsh::tokenize "'a$b'"))))
  (is (equal '("literal$x")
             (word-texts (sbsh::tokenize "literal\\$x")))))

(test tokenize-operators
  (let ((toks (sbsh::tokenize "a | b && c || d ; e &")))
    (is (member :pipe toks))
    (is (member :and toks))
    (is (member :or toks))
    (is (member :semi toks))
    (is (member :amp toks))))

(test tokenize-redirections
  (let ((toks (sbsh::tokenize "cmd < in > out 2>> err 2>&1 >> app")))
    (is (equal '(:redir :in 0)     (find-if (lambda (x) (and (consp x) (eq (third x) 0))) toks)))
    (is (member '(:redir :out 1)    toks :test #'equal))
    (is (member '(:redir :append 2) toks :test #'equal))
    (is (member '(:redir :dup 2 1)  toks :test #'equal))
    (is (member '(:redir :append 1) toks :test #'equal))))

;;; --- Expansion ----------------------------------------------------------

(test tilde-expansion
  (is (string= (namestring (user-homedir-pathname))
               (sbsh::expand-tilde "~")))
  (is (string= "/etc/passwd" (sbsh::expand-tilde "/etc/passwd"))))

(test variable-expansion
  (sb-posix:setenv "SBSH_TEST_VAR" "greetings" 1)
  (is (equal '("greetings")
             (word-texts (sbsh::tokenize "$SBSH_TEST_VAR"))))
  (is (equal '("greetings-x")
             (word-texts (sbsh::tokenize "${SBSH_TEST_VAR}-x"))))
  (is (equal '("[greetings]")
             (word-texts (sbsh::tokenize "\"[$SBSH_TEST_VAR]\"")))))

(test status-variable
  (let ((sbsh::*last-status* 42))
    (is (equal '("42") (word-texts (sbsh::tokenize "$?"))))))

;;; --- Parser -------------------------------------------------------------

(test parse-simple
  (let ((clauses (sbsh::parse-line "echo hi")))
    (is (= 1 (length clauses)))
    (let ((cmd (first (sbsh::pipeline-commands
                       (sbsh::clause-pipeline (first clauses))))))
      (is (equal '("echo" "hi") (sbsh::command-argv cmd))))))

(test parse-pipeline
  (let* ((clauses (sbsh::parse-line "ls -l | grep foo | wc -l"))
         (pl (sbsh::clause-pipeline (first clauses))))
    (is (= 3 (length (sbsh::pipeline-commands pl))))))

(test parse-connectors
  (let ((clauses (sbsh::parse-line "a && b || c ; d")))
    (is (= 4 (length clauses)))
    (is (eq :and (sbsh::clause-connector (first clauses))))
    (is (eq :or  (sbsh::clause-connector (second clauses))))
    (is (eq :seq (sbsh::clause-connector (third clauses))))))

(test parse-background
  (let ((clauses (sbsh::parse-line "sleep 5 &")))
    (is-true (sbsh::pipeline-background (sbsh::clause-pipeline (first clauses))))))

(test parse-redirection
  (let* ((clauses (sbsh::parse-line "echo hi > /tmp/out.txt"))
         (cmd (first (sbsh::pipeline-commands
                      (sbsh::clause-pipeline (first clauses))))))
    (is (equal '("echo" "hi") (sbsh::command-argv cmd)))
    (is (equal '((:out 1 "/tmp/out.txt")) (sbsh::command-redirs cmd)))))

;;; --- Assignments & tilde-in-args ----------------------------------------

(test assignment-word-detection
  (is-true  (sbsh::assignment-word-p "FOO=bar"))
  (is-true  (sbsh::assignment-word-p "_x=1"))
  (is-true  (sbsh::assignment-word-p "A1=v"))
  (is-false (sbsh::assignment-word-p "=bar"))
  (is-false (sbsh::assignment-word-p "1FOO=bar"))
  (is-false (sbsh::assignment-word-p "no-equals"))
  (is-false (sbsh::assignment-word-p "a-b=c")))

(test tilde-in-argument-words
  ;; Tilde expansion happens in EXPAND-WORDS, at execution time.
  (let ((home (namestring (user-homedir-pathname))))
    (is (equal (list home) (sbsh::expand-words (sbsh::tokenize "~"))))
    ;; A quoted tilde stays literal.
    (is (equal '("~") (sbsh::expand-words (sbsh::tokenize "'~'"))))))

;;; --- Comments & clause splitting ----------------------------------------

(test comment-stripping
  (is (string= "echo hi " (sbsh::strip-comment "echo hi # a comment ; rm x")))
  (is (string= "echo x" (sbsh::strip-comment "echo x")))
  (is (string= "" (sbsh::strip-comment "# whole-line comment")))
  ;; # inside quotes or mid-word is not a comment
  (is (string= "echo '# literal'" (sbsh::strip-comment "echo '# literal'")))
  (is (string= "echo ab#cd" (sbsh::strip-comment "echo ab#cd"))))

(test input-completeness
  (is (null (sbsh::incomplete-reason "echo hi")))
  (is (null (sbsh::incomplete-reason "sleep 5 &")))        ; background: complete
  (is (null (sbsh::incomplete-reason "echo a; echo b")))
  (is (null (sbsh::incomplete-reason "(+ 1 2)")))
  (is (null (sbsh::incomplete-reason "echo \"closed\"")))
  (is (eq :quote     (sbsh::incomplete-reason "echo 'open")))
  (is (eq :quote     (sbsh::incomplete-reason "echo \"open")))
  (is (eq :paren     (sbsh::incomplete-reason "(progn")))
  (is (eq :backslash (sbsh::incomplete-reason "echo hi \\")))
  (is (eq :operator  (sbsh::incomplete-reason "echo hi |")))
  (is (eq :operator  (sbsh::incomplete-reason "a &&")))
  (is (eq :operator  (sbsh::incomplete-reason "a ||")))
  ;; quotes/parens inside a comment do not count as open
  (is (null (sbsh::incomplete-reason "echo hi # a ' ( quote"))))

(test logical-line-assembly
  (flet ((feed (lines)
           (let ((rest (rest lines)))
             (sbsh::assemble-logical-line
              (first lines)
              (lambda (reason) (declare (ignore reason))
                (if rest (pop rest) :eof))))))
    (is (string= "echo one two"
                 (feed '("echo one \\" "two"))))            ; backslash joins directly
    (is (string= (format nil "echo 'a~%b'")
                 (feed '("echo 'a" "b'"))))                 ; quote joins with newline
    (is (string= (format nil "(progn~%(+ 1 2))")
                 (feed '("(progn" "(+ 1 2))"))))            ; paren joins with newline
    (is (string= (format nil "echo a |~%tr a-z A-Z")
                 (feed '("echo a |" "tr a-z A-Z"))))))      ; operator joins with newline

(test multiline-parses-and-runs
  ;; A joined multi-line pipeline parses to a single 2-stage pipeline.
  (let* ((clauses (sbsh::parse-line (format nil "echo hi |~%tr a-z A-Z")))
         (cmds (sbsh::pipeline-commands (sbsh::clause-pipeline (first clauses)))))
    (is (= 1 (length clauses)))
    (is (= 2 (length cmds)))
    (is (equal '("echo" "hi") (sbsh::command-argv (first cmds))))))

(test dup-redirection-not-a-separator
  ;; The & in 2>&1 must not be treated as a background separator.
  (let ((clauses (sbsh::split-clauses "ls 2>&1 | grep x")))
    (is (= 1 (length clauses)))
    (is (null (getf (first clauses) :terminator))))
  ;; A genuine trailing & still means background.
  (let ((clauses (sbsh::split-clauses "sleep 5 &")))
    (is (= 1 (length clauses)))
    (is (eq :amp (getf (first clauses) :terminator)))))

;;; --- History ------------------------------------------------------------

(test history-dedup
  (let ((sbsh::*history* (make-array 0 :adjustable t :fill-pointer 0))
        (sbsh::*history-persist* nil))   ; keep it in memory, no disk I/O
    (sbsh::history-add "one")
    (sbsh::history-add "two")
    (sbsh::history-add "two")            ; consecutive duplicate: ignored
    (sbsh::history-add "   ")            ; blank: ignored
    (sbsh::history-add "three")
    (is (= 3 (sbsh::history-count)))
    (is (string= "one"   (sbsh::history-ref 0)))
    (is (string= "two"   (sbsh::history-ref 1)))
    (is (string= "three" (sbsh::history-ref 2)))))

(test history-search
  (let ((sbsh::*history* (make-array 0 :adjustable t :fill-pointer 0)))
    (vector-push-extend "git status" sbsh::*history*)
    (vector-push-extend "git commit" sbsh::*history*)
    (vector-push-extend "ls -la" sbsh::*history*)
    (is (= 1 (sbsh::history-search-backward "commit" 2)))
    (is (= 1 (sbsh::history-search-backward "git" 1)))
    (is (null (sbsh::history-search-backward "zzz" 2)))))

;;; --- Common Lisp extensions ---------------------------------------------

(test levenshtein-distance
  (is (= 0 (sbsh::levenshtein "echo" "echo")))
  (is (= 1 (sbsh::levenshtein "ecgo" "echo")))
  (is (= 2 (sbsh::levenshtein "gti" "git")))     ; two adjacent substitutions
  (is (= 3 (sbsh::levenshtein "kitten" "sitting"))))

(test lisp-stage-detection
  (is-true  (sbsh::lisp-stage-p "(sort lines)"))
  (is-true  (sbsh::lisp-stage-p "   (+ 1 2)"))
  (is-false (sbsh::lisp-stage-p "ls -l"))
  (is-false (sbsh::lisp-stage-p "echo (")))

(test pipeline-stage-splitting
  ;; A | inside parens (a Lisp form or $()) must not split the pipeline.
  (is (equal '("a " " b " " c") (sbsh::split-pipeline-stages "a | b | c")))
  (is (= 1 (length (sbsh::split-pipeline-stages "(logior a b)"))))
  (is (= 2 (length (sbsh::split-pipeline-stages "ls | (sort lines)")))))

(test lisp-stage-parsing
  (let* ((clauses (sbsh::parse-line "ls | (sort lines)"))
         (cmds (sbsh::pipeline-commands (sbsh::clause-pipeline (first clauses)))))
    (is (= 2 (length cmds)))
    (is (null (sbsh::command-lisp (first cmds))))
    (is (equal '(sbsh-user::sort sbsh-user::lines) (sbsh::command-lisp (second cmds))))))

(test alias-expansion
  (let ((sbsh::*aliases* (make-hash-table :test 'equal)))
    (sbsh::defalias "ll" "ls -laF")
    (let* ((clauses (sbsh::parse-line "ll /tmp"))
           (cmd (first (sbsh::pipeline-commands
                        (sbsh::clause-pipeline (first clauses))))))
      (is (equal '("ls" "-laF" "/tmp") (sbsh::command-argv cmd))))))

(test structured-history-query
  (let ((sbsh::*history-records* (make-array 0 :adjustable t :fill-pointer 0))
        (sbsh::*history-persist* nil))
    (vector-push-extend (list :text "false" :status 1 :commands '(("false")))
                        sbsh::*history-records*)
    (vector-push-extend (list :text "echo hi" :status 0 :commands '(("echo" "hi")))
                        sbsh::*history-records*)
    (vector-push-extend (list :text "grep x f" :status 0 :commands '(("grep" "x" "f")))
                        sbsh::*history-records*)
    (is (= 1 (length (sbsh::history-where #'sbsh::failed-p))))
    (is (equal '("echo hi")
               (mapcar #'sbsh::entry-text
                       (sbsh::history-where
                        (lambda (e) (sbsh::command-used-p "echo" e))))))))

(test not-found-does-not-abort-line
  ;; A command-not-found in one clause must not skip later ;-clauses.
  (let ((sbsh::*interactive* nil)
        (sbsh::*history-persist* nil)
        (sbsh::*last-status* 0)
        (sbsh::*history-records* (make-array 0 :adjustable t :fill-pointer 0))
        (*error-output* (make-broadcast-stream)))  ; discard the error text
    (sbsh::execute-line "nosuchcmd_zzz; true")
    (is (= 0 sbsh::*last-status*))          ; the `true` after ; still ran
    (sbsh::execute-line "nosuchcmd_zzz && echo no")
    (is (= 127 sbsh::*last-status*))))      ; && short-circuited, stayed 127

(test heredoc-delimiter-reading
  (multiple-value-bind (d q i) (sbsh::read-heredoc-delimiter "EOF rest" 0)
    (is (string= "EOF" d)) (is (null q)) (is (= 3 i)))
  (multiple-value-bind (d q) (sbsh::read-heredoc-delimiter "'EOF'" 0)
    (is (string= "EOF" d)) (is-true q))
  (multiple-value-bind (d q) (sbsh::read-heredoc-delimiter "\"END\"" 0)
    (is (string= "END" d)) (is-true q)))

(test heredoc-scanning
  (is (equal '(("EOF" . nil)) (sbsh::scan-heredocs "cat <<EOF")))
  (is (equal '(("END" . t))   (sbsh::scan-heredocs "cat <<-END")))
  (is (equal '(("A" . nil) ("B" . nil)) (sbsh::scan-heredocs "cat <<A <<B")))
  ;; <<< is a here-string, not a heredoc; << inside quotes does not count
  (is (null (sbsh::scan-heredocs "cat <<< word")))
  (is (null (sbsh::scan-heredocs "echo '<<EOF'"))))

(test heredoc-body-collection
  ;; Two heredocs, bodies read in order until their delimiters.
  (let ((lines (list "one" "two" "A" "three" "B" "leftover")))
    (is (equal '("one
two
" "three
")
               (sbsh::collect-heredoc-bodies
                "cat <<A <<B"
                (lambda (delim) (declare (ignore delim))
                  (if lines (pop lines) :eof)))))))

(test heredoc-dedent
  ;; <<- strips leading tabs from body lines and the terminator.
  (let ((lines (list (format nil "~Cindented" #\Tab) "END")))
    (is (equal '("indented
")
               (sbsh::collect-heredoc-bodies
                "cat <<-END"
                (lambda (delim) (declare (ignore delim))
                  (if lines (pop lines) :eof)))))))

(test heredoc-body-expansion
  (sb-posix:setenv "SBSH_HD" "xyz" 1)
  (is (string= "val xyz done" (sbsh::expand-heredoc-body "val $SBSH_HD done")))
  (is (string= "$SBSH_HD kept" (sbsh::expand-heredoc-body "\\$SBSH_HD kept"))))

(test balanced-parens-reader
  (multiple-value-bind (inner after) (sbsh::read-balanced-parens "(a (b) c)xyz" 0)
    (is (string= "a (b) c" inner))
    (is (= 9 after))))

;;; --- Line editor helpers ------------------------------------------------

(test visible-length-strips-ansi
  (is (= 5 (sbsh::visible-length "hello")))
  (is (= 5 (sbsh::visible-length (format nil "~C[1;32mhello~C[0m" #\Escape #\Escape)))))

(test longest-common-prefix
  (is (string= "fo" (sbsh::longest-common-prefix '("foo" "foobar" "fox"))))
  (is (string= ""   (sbsh::longest-common-prefix '("abc" "xyz"))))
  (is (string= "one" (sbsh::longest-common-prefix '("one")))))

(test editor-insert-delete
  (let ((ed (sbsh::make-led "$ ")))
    (loop for c across "helo" do (sbsh::ed-insert ed c))
    (is (string= "helo" (sbsh::ed-text ed)))
    (setf (sbsh::led-point ed) 3)
    (sbsh::ed-insert ed #\l)
    (is (string= "hello" (sbsh::ed-text ed)))
    (sbsh::ed-delete-back ed)
    (is (string= "helo" (sbsh::ed-text ed)))))
