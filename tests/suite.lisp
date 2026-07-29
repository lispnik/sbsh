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

(test fnmatch-posix-classes
  (is-true  (sbsh::fnmatch "[[:digit:]]" "5"))
  (is-false (sbsh::fnmatch "[[:digit:]]" "a"))
  (is-true  (sbsh::fnmatch "[[:alpha:]]" "Q"))
  (is-false (sbsh::fnmatch "[[:alpha:]]" "3"))
  (is-true  (sbsh::fnmatch "file[[:digit:]]" "file7"))
  (is-true  (sbsh::fnmatch "[[:upper:]]" "A"))
  (is-false (sbsh::fnmatch "[[:upper:]]" "a"))
  (is-true  (sbsh::fnmatch "[[:space:]]" " ")))

(test readwrite-and-noclobber-redirs
  ;; <> parses as a read-write redirection; >| parses as truncating output
  (let ((cmd (first (sbsh::pipeline-commands
                     (sbsh::clause-pipeline (first (sbsh::parse-line "cat <>/tmp/x")))))))
    (is (equal '((:readwrite 0 "/tmp/x")) (sbsh::command-redirs cmd))))
  (let ((cmd (first (sbsh::pipeline-commands
                     (sbsh::clause-pipeline (first (sbsh::parse-line "echo hi >|/tmp/y")))))))
    (is (equal '(("echo" "hi")) (list (sbsh::command-argv cmd))))
    ;; >| is a force-truncate redirection (:clobber), distinct from > (#37).
    (is (equal '((:clobber 1 "/tmp/y")) (sbsh::command-redirs cmd)))))

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
  ;; Bare ~ expands WITHOUT a trailing slash (POSIX / dash / bash), see #47.
  (is (string= (string-right-trim "/" (namestring (user-homedir-pathname)))
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
  (let ((home (string-right-trim "/" (namestring (user-homedir-pathname)))))
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

(test positional-parameters
  (let ((sbsh::*positional* '("a" "b" "c")))
    (is (equal '("a") (sbsh::expand-words (sbsh::tokenize "$1"))))
    (is (equal '("c") (sbsh::expand-words (sbsh::tokenize "$3"))))
    (is (equal '()  (sbsh::expand-words (sbsh::tokenize "$9"))))   ; unset -> no word
    (is (equal '("3") (sbsh::expand-words (sbsh::tokenize "$#"))))
    ;; "$@" expands to one word per positional parameter
    (is (equal '("a" "b" "c") (word-texts (sbsh::tokenize "\"$@\""))))))

(test function-def-parsing
  (multiple-value-bind (name body) (sbsh::parse-function-def "foo() { echo hi; }")
    (is (string= "foo" name))
    (is (search "echo hi" body)))
  (multiple-value-bind (name body) (sbsh::parse-function-def "function bar { echo bye; }")
    (is (string= "bar" name))
    (is (search "echo bye" body)))
  (is (null (sbsh::parse-function-def "echo hello")))
  (is (null (sbsh::parse-function-def "foo | bar"))))

(test brace-group-parsing
  (is (search "echo a" (sbsh::parse-brace-group "{ echo a; echo b; }")))
  (is (null (sbsh::parse-brace-group "echo a")))
  (is (null (sbsh::parse-brace-group "{echo}"))))   ; no space => brace expansion, not a group

(test extract-brace-body-nesting
  (is (string= " a { b } c " (sbsh::extract-brace-body "{ a { b } c }" 0))))

(test newline-command-separator
  (is (= 2 (length (sbsh::split-clauses (format nil "echo a~%echo b")))))
  ;; a dangling pipe across a newline stays one clause (continuation)
  (is (= 1 (length (sbsh::split-clauses (format nil "echo a |~%tr a-z A-Z")))))
  ;; ; and newline inside a { } group are not top-level separators
  (is (= 1 (length (sbsh::split-clauses "{ echo a; echo b; }")))))

(test brace-incompleteness
  (is (eq :brace (sbsh::incomplete-reason "foo() {")))
  (is (eq :brace (sbsh::incomplete-reason "{ echo a")))
  (is (null (sbsh::incomplete-reason "{ echo a; }")))
  (is (null (sbsh::incomplete-reason "echo ${HOME}"))))   ; ${...} is not a group

(test compound-detection
  (is-true  (sbsh::compound-stage-p "if x; then y; fi"))
  (is-true  (sbsh::compound-stage-p "while c; do b; done"))
  (is-true  (sbsh::compound-stage-p "for x in a b; do y; done"))
  (is-true  (sbsh::compound-stage-p "case $x in a) b;; esac"))
  (is-false (sbsh::compound-stage-p "echo hi"))
  (is-false (sbsh::compound-stage-p "iffy"))          ; not the keyword `if`
  (is-false (sbsh::compound-stage-p "echo if")))

(test compound-completeness
  (is (eq :compound (sbsh::incomplete-reason "if true; then")))
  (is (eq :compound (sbsh::incomplete-reason "while c; do echo x")))
  (is (eq :compound (sbsh::incomplete-reason "for x in a b c")))
  (is (null (sbsh::incomplete-reason "if true; then echo hi; fi")))
  (is (null (sbsh::incomplete-reason "for x in a; do echo $x; done"))))

(test compound-not-split-by-clauses
  ;; ; inside a compound must not split it into top-level clauses
  (is (= 1 (length (sbsh::split-clauses "if a; then b; fi"))))
  (is (= 2 (length (sbsh::split-clauses "if a; then b; fi; echo c"))))
  ;; nested compound
  (is (= 1 (length (sbsh::split-clauses
                    "for x in 1 2; do if [ $x -eq 1 ]; then echo one; fi; done")))))

(test keyword-scanning
  (multiple-value-bind (w s e) (sbsh::scan-to-keyword "cond; then body; fi" 0 '("then"))
    (declare (ignore e))
    (is (string= "then" w))
    (is (= 6 s)))
  ;; `in` is found after the (non-command-position) case word
  (multiple-value-bind (w s) (sbsh::scan-to-keyword "cat in dog) x;;" 0 '("in"))
    (declare (ignore s))
    (is (string= "in" w))))

(test case-clause-splitting
  (let ((clauses (sbsh::split-case-clauses " cat|dog) echo pet ;; *) echo other ;; ")))
    (is (= 2 (length clauses)))
    (is (equal '("cat" "dog") (car (first clauses))))
    (is (search "echo pet" (cdr (first clauses))))
    (is (equal '("*") (car (second clauses))))))

(test shell-test-builtin
  (is (= 0 (sbsh::shell-test '("nonempty"))))
  (is (= 1 (sbsh::shell-test '(""))))
  (is (= 0 (sbsh::shell-test '("-z" ""))))
  (is (= 1 (sbsh::shell-test '("-n" ""))))
  (is (= 0 (sbsh::shell-test '("-e" "/etc/hosts"))))
  (is (= 1 (sbsh::shell-test '("-d" "/etc/hosts"))))
  (is (= 0 (sbsh::shell-test '("abc" "=" "abc"))))
  (is (= 1 (sbsh::shell-test '("abc" "=" "xyz"))))
  (is (= 0 (sbsh::shell-test '("3" "-lt" "5"))))
  (is (= 1 (sbsh::shell-test '("5" "-lt" "3"))))
  (is (= 0 (sbsh::shell-test '("!" "-e" "/no/such/path/xyz")))))

(test pipeline-negation-parsing
  (let ((pl (sbsh::clause-pipeline (first (sbsh::parse-line "! false")))))
    (is-true (sbsh::pipeline-negate pl))
    (is (equal '("false")
               (sbsh::command-argv (first (sbsh::pipeline-commands pl))))))
  (let ((pl (sbsh::clause-pipeline (first (sbsh::parse-line "true")))))
    (is-false (sbsh::pipeline-negate pl))))

(test pipestatus-expansion
  (let ((sbsh::*pipestatus* '(1 0 2)))
    ;; unquoted expansion is word-split; quoted keeps it as one word
    (is (equal '("1" "0" "2") (sbsh::expand-words (sbsh::tokenize "$PIPESTATUS"))))
    (is (equal '("1 0 2") (sbsh::expand-words (sbsh::tokenize "\"$PIPESTATUS\""))))
    (is (equal '("1") (sbsh::expand-words (sbsh::tokenize "${PIPESTATUS[0]}"))))
    (is (equal '("2") (sbsh::expand-words (sbsh::tokenize "${PIPESTATUS[2]}"))))
    (is (equal '() (sbsh::expand-words (sbsh::tokenize "${PIPESTATUS[9]}"))))))

(test assignment-tilde-expansion
  (let ((home (string-right-trim "/" (namestring (user-homedir-pathname)))))
    (is (string= (concatenate 'string home "/bin")
                 (sbsh::expand-assignment-value "~/bin")))
    (is (string= (concatenate 'string home "/a:" home "/b")
                 (sbsh::expand-assignment-value "~/a:~/b")))
    (is (string= "plain" (sbsh::expand-assignment-value "plain")))))

(test word-splitting
  (sb-posix:setenv "SBSH_WS" "a b c" 1)
  ;; unquoted expansion splits on IFS; quoted stays one word
  (is (equal '("a" "b" "c") (sbsh::expand-words (sbsh::tokenize "$SBSH_WS"))))
  (is (equal '("a b c") (sbsh::expand-words (sbsh::tokenize "\"$SBSH_WS\""))))
  ;; a prefix joins the first field
  (sb-posix:setenv "SBSH_WS2" "x y" 1)
  (is (equal '("px" "y") (sbsh::expand-words (sbsh::tokenize "p$SBSH_WS2"))))
  ;; assignment RHS is not split
  (is (equal '("v=x y") (sbsh::expand-words (sbsh::tokenize "v=$SBSH_WS2"))))
  (sb-posix:unsetenv "SBSH_WS") (sb-posix:unsetenv "SBSH_WS2"))

(test param-expansion-modifiers
  (sb-posix:unsetenv "SBSH_U")
  (is (equal '("default") (sbsh::expand-words (sbsh::tokenize "${SBSH_U:-default}"))))
  (sb-posix:setenv "SBSH_S" "val" 1)
  (is (equal '("val") (sbsh::expand-words (sbsh::tokenize "${SBSH_S:-default}"))))
  (is (equal '("yes") (sbsh::expand-words (sbsh::tokenize "${SBSH_S:+yes}"))))
  (is (equal '("3") (sbsh::expand-words (sbsh::tokenize "${#SBSH_S}"))))  ; length of "val"
  (sb-posix:unsetenv "SBSH_S"))

(test string-operations
  (sb-posix:setenv "SBSH_SO" "foobar.txt" 1)
  (flet ((e (s) (first (sbsh::expand-words (sbsh::tokenize s)))))
    (is (string= "bar.txt" (e "${SBSH_SO#foo}")))    ; remove prefix
    (is (string= "foobar" (e "${SBSH_SO%.txt}")))     ; remove suffix
    (sb-posix:setenv "SBSH_PATH" "/a/b/c" 1)
    (is (string= "c" (e "${SBSH_PATH##*/}")))          ; longest prefix
    (is (string= "/a/b" (e "${SBSH_PATH%/*}")))        ; shortest suffix
    (sb-posix:setenv "SBSH_R" "aaa" 1)
    (is (string= "baa" (e "${SBSH_R/a/b}")))           ; replace first
    (is (string= "bbb" (e "${SBSH_R//a/b}")))          ; replace all
    (sb-posix:setenv "SBSH_AB" "abcdef" 1)
    (is (string= "cdef" (e "${SBSH_AB:2}")))           ; substring offset
    (is (string= "cd" (e "${SBSH_AB:2:2}"))))          ; substring off+len
  (sb-posix:unsetenv "SBSH_SO") (sb-posix:unsetenv "SBSH_R"))

(test nested-brace-expansion
  (is (= 7 (sbsh::find-matching-brace "{a:-{b}}xyz" 0)))    ; matches the outer }
  (sb-posix:setenv "SBSH_NB" "" 1)
  (sb-posix:setenv "SBSH_NB2" "deep" 1)
  ;; nested ${...:-${...}} resolves the inner default
  (is (equal '("deep")
             (sbsh::expand-words (sbsh::tokenize "${SBSH_NB:-${SBSH_NB2}}"))))
  (sb-posix:unsetenv "SBSH_NB") (sb-posix:unsetenv "SBSH_NB2"))

(test star-join-uses-ifs
  (let ((sbsh::*positional* '("a" "b" "c")))
    (sb-posix:unsetenv "IFS")
    (is (string= "a b c" (sbsh::star-join sbsh::*positional*)))  ; default: space
    (sb-posix:setenv "IFS" ",:" 1)
    (is (string= "a,b,c" (sbsh::star-join sbsh::*positional*))))  ; first IFS char
  (sb-posix:unsetenv "IFS"))

(test read-field-assignment
  (sb-posix:unsetenv "IFS")
  ;; one var gets the whole line (trimmed ends, internal whitespace preserved)
  (sbsh::assign-read-vars '("RV") "  hi  there  ")
  (is (string= "hi  there" (sbsh::getenv "RV")))
  ;; two vars: first field, then unsplit remainder
  (sbsh::assign-read-vars '("RA" "RB") "  a   b   c  ")
  (is (string= "a" (sbsh::getenv "RA")))
  (is (string= "b   c" (sbsh::getenv "RB")))
  (dolist (v '("RV" "RA" "RB")) (sb-posix:unsetenv v)))

(test namestring-unescaping
  ;; SBCL escapes glob metacharacters in namestrings; we undo that so a real
  ;; file named c*d globs back to itself.
  (is (string= "c*d" (sbsh::unescape-namestring "c\\*d")))
  (is (string= "a?b" (sbsh::unescape-namestring "a\\?b")))
  (is (string= "plain.txt" (sbsh::unescape-namestring "plain.txt")))
  (is (string= "a b" (sbsh::unescape-namestring "a b"))))

(test prefix-assignment-scope
  ;; VAR=val cmd sets VAR only for that command; a bare VAR=val persists.
  (sb-posix:unsetenv "SBSH_PA")
  (let ((*error-output* (make-broadcast-stream)))
    (sbsh::execute-line "SBSH_PA=temp true"))
  (is (null (sbsh::getenv "SBSH_PA")))          ; not persisted
  (sbsh::execute-line "SBSH_PA=perm")
  (is (string= "perm" (sbsh::getenv "SBSH_PA")))  ; bare assignment persists
  (sb-posix:unsetenv "SBSH_PA"))

(test path-normalization
  (is (string= "/a/b/c" (sbsh::normalize-path "/a/b/c")))
  (is (string= "/a/c" (sbsh::normalize-path "/a/b/../c")))
  (is (string= "/a" (sbsh::normalize-path "/a/b/..")))
  (is (string= "/" (sbsh::normalize-path "/a/..")))
  (is (string= "/" (sbsh::normalize-path "/../..")))
  (is (string= "/a/b" (sbsh::normalize-path "/a/./b/")))
  (is (string= "/x/y" (sbsh::normalize-path "//x///y//"))))

(test empty-field-preservation
  ;; empty unquoted expansion yields no word...
  (sb-posix:setenv "SBSH_E" "" 1)
  (is (equal '() (sbsh::expand-words (sbsh::tokenize "$SBSH_E"))))
  ;; ...but empty fields from a non-whitespace IFS are kept
  (sb-posix:setenv "SBSH_C" "a,,b" 1)
  (sb-posix:setenv "IFS" "," 1)
  (is (equal '("a" "" "b") (sbsh::expand-words (sbsh::tokenize "$SBSH_C"))))
  (sb-posix:unsetenv "IFS")
  (sb-posix:unsetenv "SBSH_E") (sb-posix:unsetenv "SBSH_C"))

(test ifs-splitting
  ;; default (whitespace) IFS collapses runs and trims
  (let ((sbsh::*positional* '()))
    (is (equal '("a" "b" "c") (sbsh::ifs-split "  a   b  c " " "))))
  ;; a non-whitespace IFS preserves empty fields
  (is (equal '("a" "b" "c") (sbsh::ifs-split "a:b:c" ":")))
  (is (equal '("a" "" "b") (sbsh::ifs-split "a::b" ":")))
  (is (equal '() (sbsh::ifs-split "   " " "))))

(test test-file-operators
  (is (= 0 (sbsh::shell-test '("-e" "/etc/hosts"))))
  (is (= 0 (sbsh::shell-test '("-f" "/etc/hosts"))))
  (is (= 1 (sbsh::shell-test '("-L" "/etc/hosts"))))       ; not a symlink
  (is (= 0 (sbsh::shell-test '("/etc/hosts" "-ef" "/etc/hosts"))))  ; same inode
  (is (= 1 (sbsh::shell-test '("/etc/hosts" "-ef" "/etc/passwd"))))
  (is (= 0 (sbsh::shell-test '("/nonexistent-a" "-ot" "/etc/hosts")))))  ; missing = older

(test param-count-and-substring
  (let ((sbsh::*positional* '("a" "b" "c" "d")))
    (is (equal '("4") (sbsh::expand-words (sbsh::tokenize "${#@}"))))
    (is (equal '("4") (sbsh::expand-words (sbsh::tokenize "${#*}")))))
  (sb-posix:setenv "SBSH_SS" "abcdef" 1)
  (flet ((e (s) (first (sbsh::expand-words (sbsh::tokenize s)))))
    (is (string= "ef" (e "${SBSH_SS: -2}")))       ; negative offset (space)
    (is (string= "ef" (e "${SBSH_SS:(-2)}")))      ; parenthesized negative
    (is (string= "cdef" (e "${SBSH_SS:2}"))))
  (sb-posix:unsetenv "SBSH_SS"))

(test test-and-or-operators
  (is (= 0 (sbsh::shell-test '("1" "-eq" "1" "-a" "2" "-eq" "2"))))
  (is (= 1 (sbsh::shell-test '("1" "-eq" "1" "-a" "2" "-eq" "3"))))
  (is (= 0 (sbsh::shell-test '("1" "-eq" "9" "-o" "2" "-eq" "2"))))
  (is (= 1 (sbsh::shell-test '("1" "-eq" "9" "-o" "2" "-eq" "8")))))

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

;;; ======================================================================
;;; Round 10 conformance: audit fixes (crashes, DoS, loop control, guards)
;;; ======================================================================

(defun run-line (s)
  "Execute S with the interpreter in-process, returning (values STDOUT STATUS).
Shell globals are freshly bound so tests do not leak state into each other."
  (let ((out (make-string-output-stream)))
    (let ((*standard-output* out)
          (sbsh::*errexit* nil) (sbsh::*nounset* nil) (sbsh::*pipefail* nil)
          (sbsh::*should-exit* nil) (sbsh::*loop-depth* 0)
          (sbsh::*positional* nil) (sbsh::*last-status* 0)
          (sbsh::*condition-context* nil) (sbsh::*line-commands* nil)
          (sbsh::*cmdsub-status* nil) (sbsh::*pipestatus* nil))
      (sbsh::run-command-line s)
      (values (get-output-stream-string out) sbsh::*last-status*))))

;;; --- #03: unterminated [ must not crash; POSIX treats it literally ------
(test fnmatch-unterminated-bracket
  (is-false (sbsh::fnmatch "[a" "a"))       ; would OOB-crash before the fix
  (is-true  (sbsh::fnmatch "[a" "[a"))      ; literal [a matches itself
  (is-false (sbsh::fnmatch "[a-" "a"))
  (is-true  (sbsh::fnmatch "[a-" "[a-"))
  ;; a properly closed class still works
  (is-true  (sbsh::fnmatch "[abc]" "b"))
  (is-false (sbsh::fnmatch "[abc]" "z")))

;;; --- #07: fnmatch must not backtrack exponentially (ReDoS) --------------
(test fnmatch-no-exponential-backtracking
  ;; Pattern *a*a*...*b vs a string of all 'a': the classic catastrophic case.
  ;; With memoization this returns immediately; without it, it hangs forever.
  (let ((pat (with-output-to-string (s)
               (dotimes (i 25) (write-string "*a" s))
               (write-string "*b" s)))
        (nm (make-string 50 :initial-element #\a)))
    (is-false (sbsh::fnmatch pat nm)))
  ;; And a matching variant still succeeds.
  (is-true (sbsh::fnmatch "*a*a*b" "xaxaxb")))

;;; --- #27 / #31: shift must reject bad counts, never crash --------------
(test shift-rejects-bad-count
  (let ((sbsh::*positional* (list "a" "b" "c")))
    (is (= 1 (sbsh::run-builtin "shift" '("-1"))))      ; was a fatal TYPE-ERROR
    (is (equal '("a" "b" "c") sbsh::*positional*)))
  (let ((sbsh::*positional* (list "a" "b" "c")))
    (is (= 1 (sbsh::run-builtin "shift" '("abc"))))     ; non-numeric
    (is (equal '("a" "b" "c") sbsh::*positional*)))
  (let ((sbsh::*positional* (list "a" "b" "c")))
    (is (= 1 (sbsh::run-builtin "shift" '("9"))))       ; over-shift: no change
    (is (equal '("a" "b" "c") sbsh::*positional*)))
  (let ((sbsh::*positional* (list "a" "b" "c")))
    (is (= 0 (sbsh::run-builtin "shift" '("2"))))
    (is (equal '("c") sbsh::*positional*))))

;;; --- #30: exit with a non-numeric argument errors (status 2) -----------
(test exit-nonnumeric-argument
  (let ((sbsh::*should-exit* nil) (sbsh::*last-status* 0))
    (is (= 2 (sbsh::run-builtin "exit" '("foo"))))
    (is (eql 2 sbsh::*should-exit*)))
  (let ((sbsh::*should-exit* nil) (sbsh::*last-status* 0))
    (is (= 5 (sbsh::run-builtin "exit" '("5"))))
    (is (eql 5 sbsh::*should-exit*)))
  (let ((sbsh::*should-exit* nil) (sbsh::*last-status* 0))
    (is (= 42 (sbsh::run-builtin "exit" '("298"))))     ; 298 mod 256 = 42
    (is (eql 42 sbsh::*should-exit*))))

;;; --- #28: integer test operators reject non-integer operands ----------
(test test-integer-rejects-noninteger
  (is (= 2 (sbsh::shell-test '("3x" "-eq" "3"))))       ; was silent partial-parse
  (is (= 2 (sbsh::shell-test '("3" "-eq" "3y"))))
  (is (= 0 (sbsh::shell-test '("3" "-eq" "3"))))
  (is (= 0 (sbsh::shell-test '("-5" "-lt" "2"))))
  (is (= 1 (sbsh::shell-test '("10" "-lt" "2")))))

;;; --- #04: set -e in a while/until body stops the loop (no spin) --------
(test errexit-breaks-while-loop
  (multiple-value-bind (out status) (run-line "set -e; while true; do false; echo body; done; echo after")
    (is (string= "" out))                                ; body/after never printed
    (is (= 1 status)))                                    ; exits with false's status
  ;; without errexit, break still terminates the loop normally
  (multiple-value-bind (out status) (run-line "while true; do echo x; break; done; echo done")
    (declare (ignore status))
    (is (string= (format nil "x~%done~%") out))))

;;; --- #25: break N / continue N honor the numeric level ----------------
(test break-continue-levels
  (is (string= (format nil "1a~%")
               (run-line "for i in 1 2 3; do for j in a b; do echo $i$j; break 2; done; done")))
  ;; break 1 only leaves the inner loop
  (is (string= (format nil "1a~%2a~%3a~%")
               (run-line "for i in 1 2 3; do for j in a b; do echo $i$j; break; done; done")))
  ;; continue 2 skips to the next outer iteration
  (is (string= ""
               (run-line "for i in 1 2; do for j in a b; do [ $j = a ] && continue 2; echo $i$j; done; echo end$i; done"))))

;;; ======================================================================
;;; Round 10 conformance: expansion & globbing
;;; ======================================================================

;;; --- #01: POSIX arithmetic $((...)), with Lisp fallback preserved ------
(test eval-arithmetic-direct
  (is (= 3   (sbsh::eval-arithmetic "1+2")))
  (is (= 20  (sbsh::eval-arithmetic "(2+3)*4")))
  (is (= 3   (sbsh::eval-arithmetic "10/3")))
  (is (= 1   (sbsh::eval-arithmetic "10%3")))
  (is (= 1   (sbsh::eval-arithmetic "3>2")))
  (is (= 0   (sbsh::eval-arithmetic "3<2")))
  (is (= 1   (sbsh::eval-arithmetic "3==3")))
  (is (= 1   (sbsh::eval-arithmetic "5 && 2")))
  (is (= 0   (sbsh::eval-arithmetic "0 || 0")))
  (is (= -1  (sbsh::eval-arithmetic "2-3")))
  (is (= 255 (sbsh::eval-arithmetic "0xff")))
  ;; expressions that are NOT valid POSIX arithmetic must signal, so the
  ;; caller can fall back to Common Lisp evaluation.
  (signals sbsh::arith-error (sbsh::eval-arithmetic "+ 1 2"))
  (signals sbsh::arith-error (sbsh::eval-arithmetic "expt 2 10"))
  (signals sbsh::arith-error (sbsh::eval-arithmetic "1 2")))

(test arithmetic-expansion
  (flet ((e (s) (first (sbsh::expand-words (sbsh::tokenize s)))))
    (is (string= "3"  (e "$((1+2))")))
    (is (string= "14" (e "$((2*(3+4)))")))
    (is (string= "1"  (e "$((10%3))")))
    (is (string= "1"  (e "$((3==3))")))
    (sb-posix:setenv "SBSH_AX" "5" 1)
    (is (string= "6"  (e "$((SBSH_AX+1))")))     ; bare name -> variable
    (is (string= "10" (e "$(($SBSH_AX*2))")))    ; $-prefixed variable
    (sb-posix:unsetenv "SBSH_AX")
    ;; documented Common Lisp arithmetic still works via fallback
    (is (string= "1024" (e "$((expt 2 10))")))))

;;; --- #06: set -u must not fire for ${x-w} ${x:-w} ${x+w} etc -----------
(test nounset-modifiers-suppressed
  (sb-posix:unsetenv "SBSH_NU")
  (let ((sbsh::*nounset* t))
    (flet ((e (s) (first (sbsh::expand-words (sbsh::tokenize s)))))
      (is (string= "def" (e "${SBSH_NU:-def}")))
      (is (string= "def" (e "${SBSH_NU-def}")))
      (is (null (e "${SBSH_NU+set}")))    ; unset + -> empty -> word drops out
      (sb-posix:setenv "SBSH_NU" "1" 1)
      (is (string= "yes" (e "${SBSH_NU:+yes}")))
      (sb-posix:unsetenv "SBSH_NU"))
    ;; a bare unset expansion still errors under nounset
    (signals sbsh::shell-error (sbsh::tokenize "$SBSH_NU"))))

;;; --- #47 / #44: tilde forms -------------------------------------------
(test tilde-expansion-forms
  (let ((home (sbsh::home-dir)))
    (is (string= home (sbsh::expand-tilde "~")))            ; no trailing slash
    (is (string= (concatenate 'string home "/x") (sbsh::expand-tilde "~/x")))
    (let ((r (sbsh::expand-tilde "~root")))                 ; ~user
      (is (or (string= r "~root") (char= (char r 0) #\/))))))

;;; --- #18: $'...' ANSI-C quoting ---------------------------------------
(test ansi-c-quoting
  (is (string= (format nil "a~Cb" #\Tab)     (sbsh::ansi-c-unescape "a\\tb")))
  (is (string= (format nil "~C" #\Newline)   (sbsh::ansi-c-unescape "\\n")))
  (is (string= "'"                           (sbsh::ansi-c-unescape "\\'")))
  (is (string= "A"                           (sbsh::ansi-c-unescape "\\x41")))
  (is (string= "A"                           (sbsh::ansi-c-unescape "\\101")))
  (let ((w (first (remove-if-not #'sbsh::word-p (sbsh::tokenize "$'a\\tb'")))))
    (is (string= (format nil "a~Cb" #\Tab) (sbsh::word-text w)))
    (is-true (sbsh::word-quoted w))))          ; quoted: no split/glob

;;; --- #52: $- reflects the current option flags ------------------------
(test dollar-dash-option-flags
  (let ((sbsh::*errexit* t) (sbsh::*nounset* t) (sbsh::*xtrace* nil)
        (sbsh::*noglob* nil) (sbsh::*noclobber* nil))
    (is (string= "eu" (sbsh::var-value "-"))))
  (let ((sbsh::*errexit* nil) (sbsh::*nounset* nil) (sbsh::*xtrace* t)
        (sbsh::*noglob* t) (sbsh::*noclobber* t))
    (is (string= "xfC" (sbsh::var-value "-")))))

;;; --- #46 / #48: backslash handling ------------------------------------
(test backslash-newline-in-double-quotes
  ;; a backslash-newline inside "..." is a line continuation (both removed)
  (is (string= "line1 line2"
               (sbsh::word-text
                (first (sbsh::tokenize (format nil "\"line1 \\~Cline2\"" #\Newline)))))))

(test trailing-backslash-kept
  ;; a lone trailing backslash is kept literally, not dropped
  (is (string= "abc\\" (sbsh::word-text (first (sbsh::tokenize "abc\\"))))))

;;; --- #02: an unquoted metachar globs even next to a quoted part --------
(test quoted-adjacent-glob-flag
  (flet ((w (s) (first (remove-if-not #'sbsh::word-p (sbsh::tokenize s)))))
    (is-true  (sbsh::word-has-glob (w "\"a\"*")))   ; "a"*  -> globs
    (is-true  (sbsh::word-quoted   (w "\"a\"*")))
    (is-true  (sbsh::word-has-glob (w "*\".c\"")))  ; *".c" -> globs
    (is-true  (sbsh::word-has-glob (w "f*\"c\"")))  ; f*"c" -> globs
    (is-true  (sbsh::word-has-glob (w "a\"1\"*")))  ; a"1"* -> globs
    (is-false (sbsh::word-has-glob (w "\"*\"")))    ; fully quoted -> literal
    (is-false (sbsh::word-has-glob (w "'*'")))
    (is-true  (sbsh::word-has-glob (w "*.c")))))

;;; --- #38: set -f (noglob) disables pathname expansion -----------------
(test noglob-disables-globbing
  (let ((sbsh::*noglob* t))
    (is (equal '("*.nope-xyz") (sbsh::expand-words (sbsh::tokenize "*.nope-xyz"))))))

;;; --- #50 / #53: set -o returns failure for an unknown option name -----
(test set-o-option-semantics
  (let ((sbsh::*noclobber* nil) (sbsh::*noglob* nil) (sbsh::*xtrace* nil))
    (is (eq t   (sbsh::set-o-option "noclobber" t)))
    (is-true sbsh::*noclobber*)
    (is (eq t   (sbsh::set-o-option "noglob" t)))
    (is (eq t   (sbsh::set-o-option "xtrace" t)))
    (is (null   (sbsh::set-o-option "nosuch" t)))))

(test set-invalid-option-status
  (let ((sbsh::*errexit* nil) (sbsh::*noglob* nil) (sbsh::*positional* nil))
    (is (= 1 (sbsh::run-builtin "set" '("+o" "nosuch"))))   ; unknown -> 1
    (is (= 0 (sbsh::run-builtin "set" '("-e"))))
    (is-true sbsh::*errexit*)
    (is (= 0 (sbsh::run-builtin "set" '("-f"))))
    (is-true sbsh::*noglob*)))

;;; ======================================================================
;;; Round 10 conformance: builtins & options
;;; ======================================================================

;;; --- #29: test/[ with ( ) grouping, composing with ! -a -o ------------
(test test-paren-grouping
  (is (= 0 (sbsh::shell-test '("(" "a" "=" "a" ")"))))
  (is (= 0 (sbsh::shell-test '("(" "a" "=" "b" "-o" "c" "=" "c" ")"))))
  (is (= 1 (sbsh::shell-test '("(" "a" "=" "b" ")" "-a" "!" "-z" "x"))))
  (is (= 0 (sbsh::shell-test '("!" "(" "a" "=" "b" ")"))))
  ;; -a / -o still compose without parens (regression guard)
  (is (= 0 (sbsh::shell-test '("1" "-eq" "1" "-a" "2" "-eq" "2"))))
  (is (= 1 (sbsh::shell-test '("1" "-eq" "1" "-a" "2" "-eq" "3"))))
  (is (= 0 (sbsh::shell-test '("1" "-eq" "9" "-o" "2" "-eq" "2")))))

;;; --- #32: readonly variables cannot be reassigned ---------------------
(test readonly-enforced
  (let ((sbsh::*readonly-vars* (make-hash-table :test 'equal)))
    (setf (gethash "SBSH_RO" sbsh::*readonly-vars*) t)
    (sb-posix:setenv "SBSH_RO" "orig" 1)
    (signals sbsh::shell-error (sbsh::apply-assignment "SBSH_RO=changed"))
    (is (string= "orig" (sbsh::getenv "SBSH_RO")))
    (sbsh::apply-assignment "SBSH_RW=ok")          ; non-readonly assigns fine
    (is (string= "ok" (sbsh::getenv "SBSH_RW")))
    (sb-posix:unsetenv "SBSH_RO") (sb-posix:unsetenv "SBSH_RW")))

;;; --- #33: 126 (not executable) vs 127 (not found) helper --------------
(test file-executable-classification
  (is-true  (sbsh::file-executable-p "/bin/sh"))
  (is-false (sbsh::file-executable-p "/etc/hosts")))

;;; --- #08: eval runs its argument in the current shell -----------------
(test eval-runs-in-process
  (is (string= (format nil "hi~%bye~%") (run-line "eval 'echo hi; echo bye'")))
  (multiple-value-bind (out status) (run-line "eval 'SBSH_EV=7'; echo $SBSH_EV")
    (declare (ignore status))
    (is (string= (format nil "7~%") out)))
  (sb-posix:unsetenv "SBSH_EV"))

;;; --- #05: set -e must not exit on a !-negated pipeline ----------------
(test errexit-exempts-negated-pipeline
  ;; `! true` yields status 1, but negation exempts it from errexit.
  (multiple-value-bind (out status) (run-line "set -e; ! true; echo survived")
    (declare (ignore status))
    (is (string= (format nil "survived~%") out)))
  ;; a plain failure still triggers errexit (regression guard)
  (multiple-value-bind (out status) (run-line "set -e; false; echo nope")
    (declare (ignore status))
    (is (string= "" out))))

;;; ======================================================================
;;; Round 10 conformance: parser syntax, fd-dup, $@/$* cluster, read, trap
;;; ======================================================================

;;; --- #22/#23/#24/#49: syntax errors around control operators ----------
(test clause-syntax-errors
  (signals sbsh::shell-parse-error (sbsh::validate-clauses (sbsh::split-clauses ";")))
  (signals sbsh::shell-parse-error (sbsh::validate-clauses (sbsh::split-clauses "&& echo b")))
  (signals sbsh::shell-parse-error (sbsh::validate-clauses (sbsh::split-clauses "echo a && && echo b")))
  (signals sbsh::shell-parse-error (sbsh::validate-clauses (sbsh::split-clauses "echo hi &&")))
  ;; leading / trailing pipe is an empty stage
  (signals sbsh::shell-parse-error (sbsh::parse-segment "| echo hi"))
  (signals sbsh::shell-parse-error (sbsh::parse-segment "echo hi |"))
  ;; valid lines do NOT error
  (finishes (sbsh::validate-clauses (sbsh::split-clauses "echo a; echo b")))
  (finishes (sbsh::validate-clauses (sbsh::split-clauses "echo a && echo b")))
  (finishes (sbsh::validate-clauses (sbsh::split-clauses (format nil "echo a~%~%echo b")))))

;;; --- #36: function definition with no space after ) -------------------
(test function-def-no-space
  (multiple-value-bind (name body) (sbsh::parse-function-def "f(){ echo hi; }")
    (is (string= "f" name))
    (is (search "echo hi" body)))
  ;; the whole definition stays one clause (the inner ; is not a separator)
  (is (= 2 (length (sbsh::split-clauses "f(){ echo hi; }; f")))))

;;; --- #20/#21/#39/#55: fd duplication and close -----------------------
(test fd-dup-and-close-tokens
  (is (member '(:redir :dup 0 3)  (sbsh::tokenize "read x <&3")   :test #'equal))
  (is (member '(:redir :dup 1 2)  (sbsh::tokenize "echo 1>&2")    :test #'equal))
  (is (member '(:redir :close 1)  (sbsh::tokenize "echo >&-")     :test #'equal))
  (is (member '(:redir :close 0)  (sbsh::tokenize "cat <&-")      :test #'equal)))

;;; --- #13/#14/#15/#16/#19/#42: "$@"/"$*" splitting & adjacency ---------
(test at-star-quoting-cluster
  (let ((sbsh::*positional* (list "a b" "c")))
    (is (equal '("a b" "c") (sbsh::expand-words (sbsh::tokenize "\"$@\""))))
    (is (equal '("a b" "c") (sbsh::expand-words (sbsh::tokenize "\"${@}\"")))))   ; #14
  (let ((sbsh::*positional* (list "p" "q")))
    (is (equal '("ap" "qb") (sbsh::expand-words (sbsh::tokenize "\"a$@b\""))))    ; #15
    (is (equal '("xp" "q")  (sbsh::expand-words (sbsh::tokenize "x\"$@\""))))     ; #16
    (is (equal '("xp" "qy") (sbsh::expand-words (sbsh::tokenize "\"x$@y\""))))    ; #19
    (is (equal '("prep" "q") (sbsh::expand-words (sbsh::tokenize "pre\"$@\"")))))  ; #42
  ;; #13: assignment value joins rather than splits
  (let ((sbsh::*positional* (list "a" "b" "c")))
    (is (equal '("v=a b c") (sbsh::expand-words (sbsh::tokenize "v=\"$*\""))))
    (is (equal '("v=a b c") (sbsh::expand-words (sbsh::tokenize "v=\"$@\""))))))

;;; --- #40/#41: read field splitting (IFS + escapes) --------------------
(test read-ifs-splitting
  ;; #41: IFS whitespace around a non-whitespace delimiter is absorbed
  (sb-posix:setenv "IFS" " :" 1)
  (sbsh::assign-read-vars '("w" "x" "y" "z") "a : b : c" nil)
  (is (string= "a" (sbsh::getenv "w")))
  (is (string= "b" (sbsh::getenv "x")))
  (is (string= "c" (sbsh::getenv "y")))
  (is (string= ""  (sbsh::getenv "z")))
  (sb-posix:unsetenv "IFS")
  ;; #40: a backslash-escaped space is literal (non-raw), so it does not split
  (sbsh::assign-read-vars '("p" "q") "a\\ b" nil)
  (is (string= "a b" (sbsh::getenv "p")))
  (is (string= ""    (sbsh::getenv "q")))
  ;; -r keeps the backslash and splits on the space
  (sbsh::assign-read-vars '("p" "q") "a\\ b" t)
  (is (string= "a\\" (sbsh::getenv "p")))
  (is (string= "b"   (sbsh::getenv "q")))
  (mapc #'sb-posix:unsetenv '("w" "x" "y" "z" "p" "q")))

;;; --- #12: backslash-quoted heredoc delimiter --------------------------
(test heredoc-backslash-delimiter
  (multiple-value-bind (delim quoted end) (sbsh::read-heredoc-delimiter "\\EOF rest" 0)
    (declare (ignore end))
    (is (string= "EOF" delim))    ; de-quoted
    (is-true quoted)))            ; body not expanded

;;; --- #09: trap name normalization -------------------------------------
(test trap-name-normalization
  (is (string= "EXIT" (sbsh::normalize-trap-name "EXIT")))
  (is (string= "EXIT" (sbsh::normalize-trap-name "0")))
  (is (string= "INT"  (sbsh::normalize-trap-name "INT")))
  (is (string= "INT"  (sbsh::normalize-trap-name "SIGINT")))
  (is (string= "TERM" (sbsh::normalize-trap-name "sigterm"))))

;;; --- #09 follow-up: EXIT trap preserves the shell's exit status --------
(test exit-trap-preserves-status
  (let ((sbsh::*traps* (make-hash-table :test 'equal))
        (sbsh::*last-status* 1) (sbsh::*should-exit* nil))
    (setf (gethash "EXIT" sbsh::*traps*) ":")     ; no-op trap
    (sbsh::run-exit-trap)
    (is (= 1 sbsh::*last-status*)))               ; status from before the trap
  (let ((sbsh::*traps* (make-hash-table :test 'equal))
        (sbsh::*last-status* 0) (sbsh::*should-exit* nil))
    (setf (gethash "EXIT" sbsh::*traps*) "exit 7") ; trap that exits wins
    (sbsh::run-exit-trap)
    (is (eql 7 sbsh::*should-exit*))))
