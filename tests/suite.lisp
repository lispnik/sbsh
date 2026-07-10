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

(test ifs-splitting
  ;; default (whitespace) IFS collapses runs and trims
  (let ((sbsh::*positional* '()))
    (is (equal '("a" "b" "c") (sbsh::ifs-split "  a   b  c " " "))))
  ;; a non-whitespace IFS preserves empty fields
  (is (equal '("a" "b" "c") (sbsh::ifs-split "a:b:c" ":")))
  (is (equal '("a" "" "b") (sbsh::ifs-split "a::b" ":")))
  (is (equal '() (sbsh::ifs-split "   " " "))))

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
