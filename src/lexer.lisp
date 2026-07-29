;;;; lexer.lisp --- Tokenizer with quoting, expansion, and globbing.

(in-package #:sbsh)

;;; A word token carries its expanded text plus whether any part was quoted
;;; (quoting suppresses later globbing).  Operators are represented as
;;; keywords, and redirections as (:REDIR type fd [target]) lists.

(defstruct (word (:constructor make-word (text &optional quoted from-split has-glob)))
  text
  (quoted nil)
  (from-split nil)    ; a field produced by IFS splitting (keep even if empty)
  (has-glob nil))     ; an UNQUOTED glob metacharacter appeared in this word,
                      ; so it globs even if another part of it was quoted (#02)

(define-condition shell-parse-error (error)
  ((message :initarg :message :reader parse-error-message))
  (:report (lambda (c s) (format s "~A" (parse-error-message c)))))

;;; --- Environment / expansion --------------------------------------------

(defun getenv (name) (sb-posix:getenv name))

(defun home-dir ()
  "The current user's home directory, without a trailing slash."
  (string-right-trim "/" (namestring (user-homedir-pathname))))

(defun user-home-dir (name)
  "Home directory of NAME via getpwnam, or NIL if unknown/unsupported."
  (ignore-errors (sb-posix:passwd-dir (sb-posix:getpwnam name))))

(defun expand-tilde (string)
  "Expand a leading ~ , ~/... , ~user , or ~user/... to a home directory."
  (cond
    ((string= string "~") (home-dir))                 ; bare ~ : no trailing slash
    ((and (> (length string) 1) (char= (char string 0) #\~)
          (char= (char string 1) #\/))
     (concatenate 'string (home-dir) (subseq string 1)))
    ;; ~user or ~user/rest
    ((and (> (length string) 1) (char= (char string 0) #\~))
     (let* ((slash (position #\/ string))
            (user (subseq string 1 slash))
            (home (user-home-dir user)))
       (if home
           (concatenate 'string (string-right-trim "/" home)
                        (if slash (subseq string slash) ""))
           string)))                                   ; unknown user: leave literal
    (t string)))

(defun var-value (name)
  "Look up a variable NAME: specials ($? $$ $# $@ $* $0), positional params
($1..), then the environment."
  (cond
    ((string= name "?") (princ-to-string *last-status*))
    ((string= name "$") (princ-to-string (sb-posix:getpid)))
    ((string= name "!") (if *last-bg-pid* (princ-to-string *last-bg-pid*) ""))
    ((string= name "-") (current-option-flags))
    ((string= name "#") (princ-to-string (length *positional*)))
    ((or (string= name "@") (string= name "*")) (star-join *positional*))
    ((string= name "0") (or (getenv "0") "sbsh"))
    ((and (plusp (length name)) (every #'digit-char-p name))
     (let ((idx (parse-integer name)))
       (if (<= 1 idx (length *positional*)) (nth (1- idx) *positional*) "")))
    ((string= name "PIPESTATUS") (format nil "~{~A~^ ~}" *pipestatus*))
    ((pipestatus-index name))
    (t (let ((v (getenv name)))
         (cond
           (v v)
           ((and *nounset* (plusp (length name))
                 (or (alpha-char-p (char name 0)) (char= (char name 0) #\_)))
            (error 'expansion-error :message (format nil "~A: unbound variable" name)))
           (t ""))))))

(defun pipestatus-index (name)
  "Return the value of PIPESTATUS[N], or NIL if NAME is not that form."
  (let ((br (position #\[ name)))
    (when (and br (plusp (length name)) (char= (char name (1- (length name))) #\])
               (string= (subseq name 0 br) "PIPESTATUS"))
      (let ((idx (parse-integer name :start (1+ br) :end (1- (length name))
                                     :junk-allowed t)))
        (if (and idx (< -1 idx (length *pipestatus*)))
            (princ-to-string (nth idx *pipestatus*))
            "")))))

(defun var-name-char-p (c)
  (or (alphanumericp c) (char= c #\_)))

(defun current-option-flags ()
  "The set -x/-e/-u/... flag letters currently in effect, for $-."
  (with-output-to-string (s)
    (when *errexit* (write-char #\e s))
    (when *nounset* (write-char #\u s))
    (when *xtrace* (write-char #\x s))
    (when *noglob* (write-char #\f s))
    (when *noclobber* (write-char #\C s))))

(defun star-join (list)
  "Join LIST with the first character of $IFS (a space by default), for $*."
  (let ((sep (let ((ifs (ifs-value))) (if (plusp (length ifs)) (string (char ifs 0)) ""))))
    (with-output-to-string (out)
      (loop for x in list for first = t then nil
            do (unless first (write-string sep out)) (write-string x out)))))

(defun strip-affix (val pat suffix longest)
  "Remove a prefix (SUFFIX nil) or suffix (SUFFIX t) of VAL matching glob PAT,
shortest match unless LONGEST."
  (let* ((n (length val))
         (ks (if suffix
                 (if longest (loop for k from 0 to n collect k)
                     (loop for k from n downto 0 collect k))
                 (if longest (loop for k from n downto 0 collect k)
                     (loop for k from 0 to n collect k)))))
    (dolist (k ks val)
      (if suffix
          (when (fnmatch pat (subseq val k)) (return (subseq val 0 k)))
          (when (fnmatch pat (subseq val 0 k)) (return (subseq val k)))))))

(defun replace-substr (val pat repl all)
  "Replace literal PAT in VAL with REPL (first, or ALL)."
  (if (or (zerop (length pat)) (not (search pat val)))
      val
      (with-output-to-string (out)
        (let ((i 0) (pl (length pat)))
          (loop
            (let ((pos (search pat val :start2 i)))
              (cond
                ((null pos) (write-string (subseq val i) out) (return))
                (t (write-string (subseq val i pos) out)
                   (write-string repl out)
                   (setf i (+ pos pl))
                   (unless all (write-string (subseq val i) out) (return))))))))))

(defun strip-parens (s)
  "Remove one layer of surrounding parentheses from S (for ${x:(-2)})."
  (let ((s (string-trim '(#\Space) s)))
    (if (and (>= (length s) 2) (char= (char s 0) #\() (char= (char s (1- (length s))) #\)))
        (subseq s 1 (1- (length s)))
        s)))

(defun substring-of (val spec)
  "Return the ${var:offset[:length]} substring of VAL; SPEC is the text after
the colon."
  (let* ((c (position #\: spec))
         (off (or (parse-integer (strip-parens (if c (subseq spec 0 c) spec)) :junk-allowed t) 0))
         (len (and c (parse-integer (subseq spec (1+ c)) :junk-allowed t)))
         (n (length val))
         (start (min n (max 0 (if (minusp off) (+ n off) off))))
         (end (if len (min n (max start (+ start len))) n)))
    (subseq val start end)))

(defun param-name-end (s)
  "Length of the parameter name at the start of S."
  (cond
    ((zerop (length s)) 0)
    ((member (char s 0) '(#\@ #\* #\? #\$ #\! #\#)) 1)
    ((digit-char-p (char s 0)) (or (position-if-not #'digit-char-p s) (length s)))
    (t (or (position-if-not #'var-name-char-p s) (length s)))))

(defun braced-var-value (content)
  "Value of a ${...} expression: length, default/alternate (:- := :+ :?),
prefix/suffix removal (# ## % %%), replacement (/ //), and substrings (:off:len)."
  (cond
    ((zerop (length content)) "")
    ((string= content "#") (var-value "#"))
    ;; ${#@} / ${#*} -> number of positional parameters
    ((or (string= content "#@") (string= content "#*"))
     (princ-to-string (length *positional*)))
    ;; ${#name} length (but not ${#} which is above, nor ${#var...ops})
    ((and (char= (char content 0) #\#) (> (length content) 1)
          (let ((c1 (char content 1))) (or (var-name-char-p c1) (member c1 '(#\@ #\*)))))
     (princ-to-string (length (var-value (subseq content 1)))))
    (t (let* ((ne (param-name-end content))
              (name (subseq content 0 ne))
              (rest (subseq content ne))
              ;; ${x-w} ${x:-w} ${x+w} ${x:=w} ${x?w} handle an unset variable
              ;; themselves, so nounset must not fire while reading its value.
              (val (let ((*nounset* (if (modifier-suppresses-nounset-p rest) nil *nounset*)))
                     (var-value name))))
         (if (zerop (length rest))
             val
             (let ((c0 (char rest 0)))
               (flet ((two (c) (and (> (length rest) 1) (char= (char rest 1) c))))
                 (cond
                   ;; # / ## remove prefix ; % / %% remove suffix
                   ((char= c0 #\#) (strip-affix val (subseq rest (if (two #\#) 2 1)) nil (two #\#)))
                   ((char= c0 #\%) (strip-affix val (subseq rest (if (two #\%) 2 1)) t (two #\%)))
                   ;; / // replace
                   ((char= c0 #\/)
                    (let* ((all (two #\/))
                           (body (subseq rest (if all 2 1)))
                           (slash (position #\/ body))
                           (pat (if slash (subseq body 0 slash) body))
                           (repl (if slash (expand-heredoc-body (subseq body (1+ slash))) "")))
                      (replace-substr val pat repl all)))
                   ;; :offset[:length] substring, or :-/:=/:+/:? defaults
                   ((char= c0 #\:)
                    (if (and (> (length rest) 1) (member (char rest 1) '(#\- #\= #\+ #\?)))
                        (apply-default val name (char rest 1) (subseq rest 2) t)
                        (substring-of val (subseq rest 1))))
                   ((member c0 '(#\- #\= #\+ #\?))
                    (apply-default val name c0 (subseq rest 1) nil))
                   ;; array subscript like PIPESTATUS[0]
                   ((char= c0 #\[) (var-value content))
                   (t val)))))))))

(defun modifier-suppresses-nounset-p (rest)
  "True when the ${name REST} modifier (- = + ? with or without a leading colon)
supplies its own unset handling, so set -u should not error while reading name."
  (and (plusp (length rest))
       (let ((c0 (char rest 0)))
         (or (member c0 '(#\- #\= #\+ #\?))
             (and (char= c0 #\:) (> (length rest) 1)
                  (member (char rest 1) '(#\- #\= #\+ #\?)))))))

(defun apply-default (val name op word colon)
  "Handle the ${var OP word} default/alternate operators."
  (let ((missing (if colon (zerop (length val)) (null (getenv name)))))
    (flet ((w () (expand-heredoc-body word)))
      (case op
        (#\- (if missing (w) val))
        (#\= (if missing (let ((v (w))) (sb-posix:setenv name v 1) v) val))
        (#\+ (if missing "" (w)))
        (#\? (if missing
                 (error 'expansion-error :message
                        (format nil "~A: ~A" name
                                (if (plusp (length word)) (w) "parameter null or not set")))
                 val))
        (t val)))))

;;; --- Globbing -----------------------------------------------------------

(defun wildcard-p (s)
  (or (find #\* s) (find #\? s) (find #\[ s)))

(defun fnmatch (pattern name)
  "Match a single path segment NAME against PATTERN (* ? and [..] classes).
Memoized on (px, nx) so `*` backtracking is O(plen*nlen) rather than
exponential -- a pattern like `*a*a*...*b` must not hang the shell."
  (let* ((plen (length pattern))
         (nlen (length name))
         ;; NIL = unknown, :yes / :no = already computed for this state.
         (memo (make-array (list (1+ plen) (1+ nlen)) :initial-element nil)))
    (labels ((m (px nx)
               (let ((cached (aref memo px nx)))
                 (if cached
                     (eq cached :yes)
                     (let ((r (compute px nx)))
                       (setf (aref memo px nx) (if r :yes :no))
                       r))))
             (compute (px nx)
               (cond
                 ((= px plen) (= nx nlen))
                 ((char= (char pattern px) #\*)
                  (or (m (1+ px) nx)
                      (and (< nx nlen) (m px (1+ nx)))))
                 ((= nx nlen) nil)
                 ((char= (char pattern px) #\?) (m (1+ px) (1+ nx)))
                 ((char= (char pattern px) #\[)
                  (multiple-value-bind (ok next) (match-class pattern px (char name nx))
                    (if next
                        (and ok (m next (1+ nx)))
                        ;; Unterminated `[`: POSIX treats it as a literal `[`.
                        (and (char= (char name nx) #\[) (m (1+ px) (1+ nx))))))
                 ((char= (char pattern px) (char name nx)) (m (1+ px) (1+ nx)))
                 (t nil))))
      (m 0 0))))

(defun match-class (pattern px ch)
  "Match character CH against a [..] class starting at PX in PATTERN.
Returns (values MATCHED-P INDEX-AFTER-CLASS)."
  (let* ((i (1+ px))
         (negate (and (< i (length pattern))
                      (member (char pattern i) '(#\! #\^)))))
    (when negate (incf i))
    (let ((matched nil) (start i) (n (length pattern)) (closed nil))
      (loop
        (when (>= i n) (return))
        (when (and (char= (char pattern i) #\]) (> i start)) (setf closed t) (return))
        (cond
          ;; POSIX character class [:name:]
          ((and (char= (char pattern i) #\[) (< (1+ i) n)
                (char= (char pattern (1+ i)) #\:))
           (let ((end (search ":]" pattern :start2 (+ i 2))))
             (if end
                 (progn
                   (when (posix-class-member (subseq pattern (+ i 2) end) ch)
                     (setf matched t))
                   (setf i (+ end 2)))
                 (progn (when (char= (char pattern i) ch) (setf matched t)) (incf i)))))
          ;; range a-z
          ((and (< (+ i 2) n) (char= (char pattern (1+ i)) #\-)
                (char/= (char pattern (+ i 2)) #\]))
           (when (char<= (char pattern i) ch (char pattern (+ i 2))) (setf matched t))
           (incf i 3))
          (t (when (char= (char pattern i) ch) (setf matched t)) (incf i))))
      (if closed
          (values (if negate (not matched) matched) (1+ i))
          ;; No closing `]` -- not a valid class; caller treats `[` literally.
          (values nil nil)))))

(defun posix-class-member (name ch)
  "True if CH belongs to the POSIX character class NAME (digit, alpha, ...)."
  (cond
    ((string= name "digit") (and (digit-char-p ch) t))
    ((string= name "alpha") (alpha-char-p ch))
    ((string= name "alnum") (alphanumericp ch))
    ((string= name "upper") (upper-case-p ch))
    ((string= name "lower") (lower-case-p ch))
    ((string= name "space") (and (member ch '(#\Space #\Tab #\Newline #\Return #\Page)) t))
    ((string= name "blank") (and (member ch '(#\Space #\Tab)) t))
    ((string= name "xdigit") (and (digit-char-p ch 16) t))
    ((string= name "print") (and (graphic-char-p ch) t))
    ((string= name "graph") (and (graphic-char-p ch) (char/= ch #\Space)))
    ((string= name "cntrl") (not (graphic-char-p ch)))
    ((string= name "punct")
     (and (graphic-char-p ch) (not (alphanumericp ch)) (char/= ch #\Space)))
    (t nil)))

(defun unescape-namestring (s)
  "Remove the backslash escapes SBCL adds before pathname metacharacters
(* ? [ \\) so a real filename like `c*d` reads back as itself."
  (if (find #\\ s)
      (with-output-to-string (out)
        (let ((i 0) (n (length s)))
          (loop while (< i n) do
            (if (and (char= (char s i) #\\) (< (1+ i) n))
                (progn (write-char (char s (1+ i)) out) (incf i 2))
                (progn (write-char (char s i) out) (incf i))))))
      s))

(defun list-dir-entries (dir)
  "Return a list of (NAME . DIRECTORY-P) for the entries of DIR (\"\" = cwd)."
  (let ((path (if (string= dir "")
                  *default-pathname-defaults*
                  (pathname (if (char= (char dir (1- (length dir))) #\/)
                                dir (concatenate 'string dir "/")))))
        (entries '()))
    (dolist (f (ignore-errors (uiop:directory-files path)))
      (let ((n (unescape-namestring (file-namestring f))))
        (when (plusp (length n)) (push (cons n nil) entries))))
    (dolist (d (ignore-errors (uiop:subdirectories path)))
      (let ((n (car (last (pathname-directory d)))))
        (when (stringp n) (push (cons n t) entries))))
    entries))

(defun glob-expand (pattern)
  "Expand a glob PATTERN to a sorted list of matching pathnames.  Returns
NIL when nothing matches (the caller then keeps the literal word)."
  (let* ((absolute (and (plusp (length pattern)) (char= (char pattern 0) #\/)))
         (segs (remove "" (split-on-char pattern #\/) :test #'string=)))
    (labels ((dot-ok (seg name)
               (or (char= (char seg 0) #\.) (not (char= (char name 0) #\.))))
             (descend (base segs)
               (if (null segs)
                   (list base)
                   (let ((seg (car segs)) (rest (cdr segs)))
                     (if (wildcard-p seg)
                         (loop for (name . dirp) in (list-dir-entries base)
                               when (and (fnmatch seg name) (dot-ok seg name))
                                 append (if rest
                                            (when dirp
                                              (descend (concatenate 'string base name "/") rest))
                                            (list (concatenate 'string base name
                                                               (if dirp "/" "")))))
                         (let ((joined (concatenate 'string base seg)))
                           (if rest
                               (when (uiop:directory-exists-p (concatenate 'string joined "/"))
                                 (descend (concatenate 'string joined "/") rest))
                               (when (probe-file joined) (list joined)))))))))
      (let ((results (descend (if absolute "/" "") segs)))
        (sort (mapcar (lambda (s) (string-right-trim "/" s)) results) #'string<)))))

;;; --- Core tokenizer -----------------------------------------------------

(defun split-on-char (string char)
  (loop with start = 0
        for pos = (position char string :start start)
        collect (subseq string start pos)
        while pos
        do (setf start (1+ pos))))

(defun read-single-quoted (string i)
  "Read a '...'-quoted region starting after the opening quote at I."
  (let ((end (position #\' string :start i)))
    (unless end (error 'shell-parse-error :message "unterminated ' quote"))
    (values (subseq string i end) (1+ end))))

(defun ansi-c-unescape (s)
  "Process the backslash escapes of a $'...' ANSI-C quoted string."
  (with-output-to-string (out)
    (let ((i 0) (n (length s)))
      (loop while (< i n) do
        (let ((c (char s i)))
          (if (and (char= c #\\) (< (1+ i) n))
              (let ((e (char s (1+ i))))
                (case e
                  (#\n (write-char #\Newline out) (incf i 2))
                  (#\t (write-char #\Tab out) (incf i 2))
                  (#\r (write-char #\Return out) (incf i 2))
                  (#\a (write-char (code-char 7) out) (incf i 2))
                  (#\b (write-char #\Backspace out) (incf i 2))
                  (#\f (write-char #\Page out) (incf i 2))
                  (#\v (write-char (code-char 11) out) (incf i 2))
                  (#\e (write-char (code-char 27) out) (incf i 2))
                  ((#\\ #\' #\") (write-char e out) (incf i 2))
                  (#\x (let ((j (+ i 2)))
                         (loop while (and (< j n) (< (- j (+ i 2)) 2)
                                          (digit-char-p (char s j) 16)) do (incf j))
                         (if (> j (+ i 2))
                             (progn (write-char (code-char (parse-integer s :start (+ i 2) :end j :radix 16)) out)
                                    (setf i j))
                             (progn (write-char #\\ out) (incf i)))))
                  (t (if (digit-char-p e 8)
                         (let ((j (1+ i)))
                           (loop while (and (< j n) (< (- j (1+ i)) 3)
                                            (digit-char-p (char s j) 8)) do (incf j))
                           (write-char (code-char (parse-integer s :start (1+ i) :end j :radix 8)) out)
                           (setf i j))
                         (progn (write-char #\\ out) (write-char e out) (incf i 2))))))
              (progn (write-char c out) (incf i))))))))

(defun read-double-quoted (string i)
  "Read a \"...\"-quoted region (with \\ escapes and $ expansion)."
  (let ((out (make-string-output-stream))
        (n (length string)))
    (loop
      (when (>= i n) (error 'shell-parse-error :message "unterminated \" quote"))
      (let ((c (char string i)))
        (cond
          ((char= c #\") (return (values (get-output-stream-string out) (1+ i))))
          ((char= c #\\)
           (let ((next (and (< (1+ i) n) (char string (1+ i)))))
             (cond
               ;; backslash-newline is a line continuation: both are removed
               ((eql next #\Newline) (incf i 2))
               ((member next '(#\" #\\ #\$ #\`)) (write-char next out) (incf i 2))
               (t (write-char c out) (incf i)))))
          ((char= c #\$)
           (multiple-value-bind (val ni) (read-variable string (1+ i))
             (write-string val out) (setf i ni)))
          (t (write-char c out) (incf i)))))))

(defun read-double-quoted-split (string i join-p)
  "Read a \"...\" region starting after the opening quote at I.  Returns
(values PARTS INDEX-AFTER).  PARTS is normally one string, but an embedded
$@ / ${@} splits it into several: each positional becomes its own field, with
the text before it joined to the previous field and the text after to the next
(POSIX \"$@\").  When JOIN-P (an assignment value), $@ joins like $* instead."
  (let ((parts '()) (out (make-string-output-stream)) (n (length string)))
    (labels ((split-ats (params)
               (cond
                 ((null params))
                 (join-p (write-string (star-join params) out))
                 (t (write-string (first params) out)
                    (dolist (p (rest params))
                      (push (get-output-stream-string out) parts)
                      (setf out (make-string-output-stream))
                      (write-string p out)))))
             (braced (want)   ; is this "${@}" / "${*}" exactly WANT at i?
               (and (char= (char string i) #\$) (< (+ i 3) n)
                    (char= (char string (1+ i)) #\{)
                    (char= (char string (+ i 2)) want)
                    (char= (char string (+ i 3)) #\}))))
      (loop
        (when (>= i n) (error 'shell-parse-error :message "unterminated \" quote"))
        (let ((c (char string i)))
          (cond
            ((char= c #\") (incf i) (return))
            ((char= c #\\)
             (let ((next (and (< (1+ i) n) (char string (1+ i)))))
               (cond
                 ((eql next #\Newline) (incf i 2))                     ; line continuation
                 ((member next '(#\" #\\ #\$ #\`)) (write-char next out) (incf i 2))
                 (t (write-char c out) (incf i)))))
            ((and (char= c #\$) (< (1+ i) n) (char= (char string (1+ i)) #\@))
             (split-ats *positional*) (incf i 2))
            ((and (char= c #\$) (< (1+ i) n) (char= (char string (1+ i)) #\*))
             (write-string (star-join *positional*) out) (incf i 2))
            ((braced #\@) (split-ats *positional*) (incf i 4))
            ((braced #\*) (write-string (star-join *positional*) out) (incf i 4))
            ((char= c #\$)
             (multiple-value-bind (val ni) (read-variable string (1+ i))
               (write-string val out) (setf i ni)))
            (t (write-char c out) (incf i))))))
    (push (get-output-stream-string out) parts)
    (values (nreverse parts) i)))

(defun read-balanced-parens (string i)
  "STRING[i] is an open paren.  Return (values INNER-TEXT INDEX-AFTER-CLOSE),
where INNER-TEXT excludes the outer parens.  Tracks nesting and quotes."
  (let ((n (length string)) (depth 0) (q nil) (start (1+ i)))
    (loop for j from i below n
          for c = (char string j)
          do (cond
               (q (when (char= c q) (setf q nil)))
               ((or (char= c #\') (char= c #\")) (setf q c))
               ((char= c #\() (incf depth))
               ((char= c #\))
                (decf depth)
                (when (zerop depth)
                  (return-from read-balanced-parens
                    (values (subseq string start j) (1+ j)))))))
    (error 'shell-parse-error :message "unterminated $( ")))

(defun find-matching-brace (string i)
  "STRING[i] is `{`; return the index of the matching `}` (nesting-aware, so
${x:-${HOME}} works), or NIL."
  (let ((depth 0) (n (length string)))
    (loop for j from i below n
          for c = (char string j)
          do (cond ((char= c #\{) (incf depth))
                   ((char= c #\}) (decf depth) (when (zerop depth) (return j)))))))

;;; --- Arithmetic expansion $((...)) --------------------------------------

(define-condition arith-error (error) ())

(defparameter *arith-levels*
  '(("||") ("&&") ("|") ("^") ("&") ("==" "!=") ("<" "<=" ">" ">=")
    ("<<" ">>") ("+" "-") ("*" "/" "%"))
  "Binary operator groups from lowest to highest precedence.")

(defun parse-arith-int (s)
  "Parse S as a decimal or 0x.. hex integer, or NIL if it is not a plain int."
  (let ((s (string-trim '(#\Space #\Tab) s)))
    (cond
      ((zerop (length s)) nil)
      ((and (> (length s) 2) (char= (char s 0) #\0) (member (char s 1) '(#\x #\X)))
       (ignore-errors (parse-integer s :start 2 :radix 16)))
      (t (ignore-errors (parse-integer s))))))

(defun arith-lex (s)
  "Tokenize an arithmetic expression: integers, operator strings, (:VAR . name)."
  (let ((toks '()) (i 0) (n (length s)))
    (loop while (< i n) do
      (let ((c (char s i)))
        (cond
          ((member c '(#\Space #\Tab #\Newline)) (incf i))
          ((digit-char-p c)
           (let ((j i))
             (if (and (char= c #\0) (< (1+ i) n) (member (char s (1+ i)) '(#\x #\X)))
                 (progn (setf j (+ i 2))
                        (loop while (and (< j n) (digit-char-p (char s j) 16)) do (incf j)))
                 (loop while (and (< j n) (digit-char-p (char s j))) do (incf j)))
             (push (or (parse-arith-int (subseq s i j)) (error 'arith-error)) toks)
             (setf i j)))
          ((or (alpha-char-p c) (char= c #\_))
           (let ((j i))
             (loop while (and (< j n) (var-name-char-p (char s j))) do (incf j))
             (push (cons :var (subseq s i j)) toks)
             (setf i j)))
          (t (let ((two (and (< (1+ i) n) (subseq s i (+ i 2)))))
               (cond
                 ((and two (member two '("==" "!=" "<=" ">=" "&&" "||" "<<" ">>")
                                   :test #'string=))
                  (push two toks) (incf i 2))
                 ((member c '(#\+ #\- #\* #\/ #\% #\( #\) #\< #\> #\! #\~ #\& #\| #\^))
                  (push (string c) toks) (incf i))
                 (t (error 'arith-error))))))))
    (nreverse toks)))

(defun apply-arith-op (op a b)
  (cond
    ((string= op "+") (+ a b)) ((string= op "-") (- a b)) ((string= op "*") (* a b))
    ((string= op "/") (if (zerop b) (error 'arith-error) (truncate a b)))
    ((string= op "%") (if (zerop b) (error 'arith-error) (rem a b)))
    ((string= op "<") (if (< a b) 1 0)) ((string= op "<=") (if (<= a b) 1 0))
    ((string= op ">") (if (> a b) 1 0)) ((string= op ">=") (if (>= a b) 1 0))
    ((string= op "==") (if (= a b) 1 0)) ((string= op "!=") (if (/= a b) 1 0))
    ((string= op "&&") (if (and (/= a 0) (/= b 0)) 1 0))
    ((string= op "||") (if (or (/= a 0) (/= b 0)) 1 0))
    ((string= op "&") (logand a b)) ((string= op "|") (logior a b))
    ((string= op "^") (logxor a b))
    ((string= op "<<") (ash a b)) ((string= op ">>") (ash a (- b)))
    (t (error 'arith-error))))

(defun eval-arithmetic (s)
  "Evaluate a POSIX integer arithmetic expression S.  A bare name is looked up
as a shell variable (unset/non-numeric -> 0).  Signals ARITH-ERROR on a syntax
error so the caller can fall back to Common Lisp evaluation."
  (let ((toks (arith-lex s)) (pos 0))
    (when (null toks) (error 'arith-error))
    (labels ((peek () (and (< pos (length toks)) (nth pos toks)))
             (nxt () (prog1 (nth pos toks) (incf pos)))
             (var-int (name)
               (let ((v (ignore-errors (let ((*nounset* nil)) (var-value name)))))
                 (or (and v (parse-arith-int v)) 0)))
             (unary ()
               (let ((tk (peek)))
                 (cond
                   ((integerp tk) (nxt) tk)
                   ((and (consp tk) (eq (car tk) :var)) (nxt) (var-int (cdr tk)))
                   ((equal tk "(") (nxt) (prog1 (binary 0)
                                           (unless (equal (peek) ")") (error 'arith-error))
                                           (nxt)))
                   ((equal tk "-") (nxt) (- (unary)))
                   ((equal tk "+") (nxt) (unary))
                   ((equal tk "!") (nxt) (if (zerop (unary)) 1 0))
                   ((equal tk "~") (nxt) (lognot (unary)))
                   (t (error 'arith-error)))))
             (binary (level)
               (if (>= level (length *arith-levels*))
                   (unary)
                   (let ((left (binary (1+ level))))
                     (loop for op = (peek)
                           while (and op (member op (nth level *arith-levels*) :test #'equal))
                           do (nxt) (setf left (apply-arith-op op left (binary (1+ level)))))
                     left))))
      (prog1 (binary 0)
        (when (< pos (length toks)) (error 'arith-error))))))   ; trailing junk

(defun arith-substitute (expr)
  "Value of $((EXPR)).  Expand $refs, then evaluate as POSIX integer arithmetic;
fall back to Common Lisp (sbsh's documented $((lisp)) form, e.g. $((expt 2 10)))
when EXPR is not valid POSIX arithmetic."
  (let ((expanded (expand-heredoc-body expr)))
    (handler-case (princ-to-string (eval-arithmetic expanded))
      (arith-error ()
        (handler-case
            (let ((*package* *user-package*))
              (handler-bind ((warning #'muffle-warning))   ; keep invalid input quiet
                (princ-to-string (eval (read-from-string
                                        (concatenate 'string "(" expanded ")"))))))
          (error () ""))))))

(defun strip-parens-once (s)
  "Strip exactly one layer of surrounding parentheses from S (the inner text
$((...)) yields once read-balanced-parens has consumed the outer $( )."
  (let ((s (string-trim '(#\Space #\Tab) s)))
    (if (and (>= (length s) 2) (char= (char s 0) #\() (char= (char s (1- (length s))) #\)))
        (subseq s 1 (1- (length s)))
        s)))

(defun read-variable (string i)
  "Read a $NAME, ${NAME}, $((...)), or $(...) reference starting after the $ at I.
Returns (values VALUE INDEX-AFTER)."
  (let ((n (length string)))
    (cond
      ((>= i n) (values "$" i))
      ;; $((...)) arithmetic expansion (distinct from $( subshell/command ))
      ((and (char= (char string i) #\() (< (1+ i) n) (char= (char string (1+ i)) #\())
       (multiple-value-bind (inner end) (read-balanced-parens string i)
         (values (arith-substitute (strip-parens-once inner)) end)))
      ((char= (char string i) #\()
       (multiple-value-bind (body end) (read-balanced-parens string i)
         (values (command-substitute body) end)))
      ((char= (char string i) #\{)
       (let ((end (find-matching-brace string i)))
         (unless end (error 'shell-parse-error :message "unterminated ${"))
         (values (braced-var-value (subseq string (1+ i) end)) (1+ end))))
      ;; $? $$ $# $@ $* $! $- and single-digit positionals $1..$9
      ((member (char string i) '(#\? #\$ #\# #\@ #\* #\! #\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
       (values (var-value (string (char string i))) (1+ i)))
      ((var-name-char-p (char string i))
       (let ((end (or (position-if-not #'var-name-char-p string :start i) n)))
         (values (var-value (subseq string i end)) end)))
      (t (values "$" i)))))

(defun read-heredoc-delimiter (string j)
  "Read a heredoc delimiter word starting at J (may be '..'/\"..\" quoted).
Returns (values DELIM QUOTED-P INDEX-AFTER)."
  (let ((n (length string)))
    (cond
      ((>= j n) (values "" nil j))
      ((or (char= (char string j) #\') (char= (char string j) #\"))
       (let* ((quote (char string j))
              (end (position quote string :start (1+ j))))
         (if end
             (values (subseq string (1+ j) end) t (1+ end))
             (values (subseq string (1+ j)) t n))))
      (t (let* ((end (or (position-if
                          (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return
                                                  #\< #\> #\| #\& #\; #\( #\))))
                          string :start j)
                         n))
                (raw (subseq string j end)))
           ;; A backslash in the delimiter (cat <<\EOF) quotes it: the body is
           ;; not expanded and the terminator is the de-quoted word (#12).
           (if (find #\\ raw)
               (values (remove #\\ raw) t end)
               (values raw nil end)))))))

(defun expand-heredoc-body (body)
  "Expand $VAR/${VAR}/$(...) and \\$ \\` \\\\ escapes in an unquoted heredoc
BODY.  No word-splitting or globbing (single and double quotes are literal)."
  (with-output-to-string (out)
    (let ((i 0) (n (length body)))
      (loop while (< i n) do
        (let ((c (char body i)))
          (cond
            ((char= c #\\)
             (let ((next (and (< (1+ i) n) (char body (1+ i)))))
               (if (member next '(#\$ #\` #\\))
                   (progn (write-char next out) (incf i 2))
                   (progn (write-char c out) (incf i)))))
            ((char= c #\$)
             (multiple-value-bind (val ni) (read-variable body (1+ i))
               (write-string val out) (setf i ni)))
            (t (write-char c out) (incf i))))))))

(defun tokenize (string)
  "Split STRING into a list of WORD structs and operator/redirection tokens.
Performs quote removal and $/~ expansion; globbing is deferred to EXPAND-WORDS."
  (let ((tokens '())
        (i 0)
        (n (length string))
        (cur nil)               ; string-output-stream for the current word
        (quoted nil)
        (has-glob nil))         ; unquoted glob metachar seen in the current word
    (labels ((ensure-cur () (unless cur (setf cur (make-string-output-stream))))
             ;; Non-destructively read the current word's text so the fd-digit
             ;; check in < / > does not consume it (echo a>file).
             (pending () (when cur
                           (let ((s (get-output-stream-string cur)))
                             (write-string s cur) s)))
             (flush ()
               (when cur
                 (push (make-word (get-output-stream-string cur) quoted nil has-glob) tokens)
                 (setf cur nil quoted nil has-glob nil)))
             (peek (k) (and (< (+ i k) n) (char string (+ i k)))))
      (loop
        (when (>= i n) (return))
        (let ((c (char string i)))
          (cond
            ((member c '(#\Space #\Tab #\Newline #\Return)) (flush) (incf i))
            ;; $'...' ANSI-C quoting: process backslash escapes, treat as quoted.
            ((and (char= c #\$) (eql (peek 1) #\'))
             (ensure-cur)
             (multiple-value-bind (text ni) (read-single-quoted string (+ i 2))
               (write-string (ansi-c-unescape text) cur) (setf quoted t i ni)))
            ((char= c #\')
             (ensure-cur)
             (multiple-value-bind (text ni) (read-single-quoted string (1+ i))
               (write-string text cur) (setf quoted t i ni)))
            ((char= c #\")
             (ensure-cur)
             ;; In an assignment value, "$@"/"$*" join rather than field-split.
             (let ((join-p (let ((sofar (pending))) (and sofar (assignment-prefix-p sofar)))))
               (multiple-value-bind (parts ni) (read-double-quoted-split string (1+ i) join-p)
                 (setf i ni quoted t)
                 (if (= (length parts) 1)
                     (write-string (first parts) cur)
                     ;; "$@" split: first field joins the prefix already in CUR,
                     ;; each middle field becomes its own word, the last stays in
                     ;; CUR so a following suffix concatenates onto it.
                     (progn
                       (write-string (first parts) cur)
                       (dolist (p (rest parts))
                         (push (make-word (get-output-stream-string cur) t) tokens)
                         (setf cur (make-string-output-stream) has-glob nil)
                         (write-string p cur)))))))
            ((char= c #\\)
             (ensure-cur)
             (cond
               ((< (1+ i) n) (write-char (char string (1+ i)) cur) (setf quoted t) (incf i 2))
               ;; a trailing unquoted backslash at end of input is kept literally
               (t (write-char #\\ cur) (incf i))))
            ;; Unquoted $@ / $* -> each positional, word-split on IFS.
            ((and (char= c #\$) (< (1+ i) n) (member (char string (1+ i)) '(#\@ #\*)))
             (flush)
             (dolist (p *positional*)
               (dolist (f (ifs-split p)) (push (make-word f) tokens)))
             (incf i 2))
            ((char= c #\$)
             (ensure-cur)
             (multiple-value-bind (val ni) (read-variable string (1+ i))
               (setf i ni)
               ;; Word-split an unquoted expansion on IFS (POSIX), except in an
               ;; assignment's value (x=$y stays one word).
               (let ((sofar (get-output-stream-string cur)))
                 (write-string sofar cur)   ; restore what we consumed to peek
                 (if (assignment-prefix-p sofar)
                     (write-string val cur)
                     (let ((fields (ifs-split val)))
                       (cond
                         ((null fields))     ; expanded to nothing
                         ((and (= (length fields) 1) (string= (first fields) val))
                          (write-string val cur))
                         ;; Multiple fields: each becomes its own word.  Mark
                         ;; them FROM-SPLIT so empty fields (a,,b under IFS=,)
                         ;; are kept rather than dropped as empty expansions.
                         (t (write-string (first fields) cur)
                            (dolist (f (rest fields))
                              (push (make-word (get-output-stream-string cur) quoted t) tokens)
                              (setf cur (make-string-output-stream) quoted nil)
                              (write-string f cur)))))))))
            ((char= c #\|)
             (flush)
             (if (eql (peek 1) #\|) (progn (push :or tokens) (incf i 2))
                 (progn (push :pipe tokens) (incf i))))
            ((char= c #\&)
             (flush)
             (if (eql (peek 1) #\&) (progn (push :and tokens) (incf i 2))
                 (progn (push :amp tokens) (incf i))))
            ((char= c #\;) (flush) (push :semi tokens) (incf i))
            ((char= c #\<)
             (let ((fd (digits-or nil (pending))))
               (when (and cur fd) (setf cur nil quoted nil has-glob nil))
               (flush)
               (cond
                 ;; <<< here-string: the following word is the (expanded) input.
                 ((and (eql (peek 1) #\<) (eql (peek 2) #\<))
                  (push (list :redir :herestring (or fd 0)) tokens) (incf i 3))
                 ;; << or <<- heredoc: the delimiter is consumed here (the body
                 ;; was already collected by the reader); QUOTED drives whether
                 ;; the body is expanded.
                 ((eql (peek 1) #\<)
                  (let ((j (+ i 2)) (strip nil))
                    (when (and (< j n) (char= (char string j) #\-)) (setf strip t) (incf j))
                    (loop while (and (< j n) (member (char string j) '(#\Space #\Tab)))
                          do (incf j))
                    (multiple-value-bind (delim quoted nj) (read-heredoc-delimiter string j)
                      (declare (ignore delim))
                      (push (list :redir :heredoc (or fd 0) quoted strip) tokens)
                      (setf i nj))))
                 ;; <> open a file for reading and writing on the fd (default 0)
                 ((eql (peek 1) #\>)
                  (push (list :redir :readwrite (or fd 0)) tokens) (incf i 2))
                 ;; <&n : duplicate input fd n onto this fd
                 ((and (eql (peek 1) #\&) (peek 2) (digit-char-p (peek 2)))
                  (push (list :redir :dup (or fd 0) (digit-char-p (peek 2))) tokens)
                  (incf i 3))
                 ;; <&- : close this input fd
                 ((and (eql (peek 1) #\&) (eql (peek 2) #\-))
                  (push (list :redir :close (or fd 0)) tokens) (incf i 3))
                 (t (push (list :redir :in (or fd 0)) tokens) (incf i)))))
            ((char= c #\>)
             (let ((fd (digits-or nil (pending))))
               (when (and cur fd) (setf cur nil quoted nil has-glob nil))
               (flush)
               (cond
                 ((eql (peek 1) #\>) (push (list :redir :append (or fd 1)) tokens) (incf i 2))
                 ;; >| forces truncation even under set -C (noclobber).
                 ((eql (peek 1) #\|) (push (list :redir :clobber (or fd 1)) tokens) (incf i 2))
                 ((and (eql (peek 1) #\&) (peek 2) (digit-char-p (peek 2)))
                  (push (list :redir :dup (or fd 1) (digit-char-p (peek 2))) tokens)
                  (incf i 3))
                 ;; >&- : close this output fd
                 ((and (eql (peek 1) #\&) (eql (peek 2) #\-))
                  (push (list :redir :close (or fd 1)) tokens) (incf i 3))
                 (t (push (list :redir :out (or fd 1)) tokens) (incf i)))))
            (t (ensure-cur)
               ;; an unquoted glob metacharacter makes this word a glob pattern
               (when (member c '(#\* #\? #\[)) (setf has-glob t))
               (write-char c cur) (incf i)))))
      (flush)
      (nreverse tokens))))

(defun ifs-value ()
  (let ((v (getenv "IFS")))
    (if v v (coerce '(#\Space #\Tab #\Newline) 'string))))

(defun ifs-split (string &optional (ifs (ifs-value)))
  "Split STRING into fields per IFS.  Runs of IFS whitespace collapse and are
trimmed at the ends; each non-whitespace IFS character delimits a field (so
adjacent ones yield empty fields).  Empty IFS means no splitting."
  (when (zerop (length ifs))
    (return-from ifs-split (if (zerop (length string)) '() (list string))))
  (let ((fields '()) (cur '()) (n (length string)) (i 0))
    (labels ((ws-p (c) (and (member c '(#\Space #\Tab #\Newline)) (find c ifs)))
             (nonws-p (c) (and (not (member c '(#\Space #\Tab #\Newline))) (find c ifs)))
             (skip-ws () (loop while (and (< i n) (ws-p (char string i))) do (incf i)))
             (emit () (push (coerce (nreverse cur) 'string) fields) (setf cur '())))
      (skip-ws)
      (loop while (< i n) do
        (let ((c (char string i)))
          (cond
            ((ws-p c) (skip-ws)
             (when (< i n)
               (cond ((nonws-p (char string i)) (emit) (incf i) (skip-ws))
                     (t (emit)))))
            ((nonws-p c) (emit) (incf i) (skip-ws))
            (t (push c cur) (incf i)))))
      (when cur (emit))
      (nreverse fields))))

(defun assignment-prefix-p (string)
  "True if STRING so far looks like NAME= (so an unquoted $ in an assignment's
value is not word-split)."
  (let ((eq (position #\= string)))
    (and eq (> eq 0)
         (let ((c0 (char string 0)))
           (and (or (alpha-char-p c0) (char= c0 #\_))
                (loop for k from 1 below eq
                      always (var-name-char-p (char string k))))))))

(defun digits-or (default string)
  "If STRING is non-NIL and all digits, return its integer value, else DEFAULT."
  (if (and string (plusp (length string)) (every #'digit-char-p string))
      (parse-integer string)
      default))

(defun maybe-tilde (text)
  "Expand a leading ~ in an unquoted word."
  (if (and (plusp (length text)) (char= (char text 0) #\~))
      (expand-tilde text)
      text))

(defun expand-words (words)
  "Apply tilde expansion and globbing to a list of WORD structs, returning a
flat list of strings.  Quoted words pass through literally (an empty quoted
word is kept); an unquoted word that expanded to nothing is dropped."
  (loop for w in words
        for text = (if (word-quoted w) (word-text w) (maybe-tilde (word-text w)))
        append (cond
                 ;; A fully-unquoted expansion that produced nothing drops out,
                 ;; unless it is a genuine (possibly empty) IFS-split field.
                 ((and (zerop (length text)) (not (word-quoted w)) (not (word-from-split w)))
                  nil)
                 ;; Glob when an UNQUOTED metacharacter appeared (even if another
                 ;; part of the word was quoted: "$dir"/*.c, "a"*, *".c"), or when
                 ;; a fully-unquoted word's text is a wildcard (e.g. from $var).
                 ((and (not *noglob*)
                       (or (word-has-glob w)
                           (and (not (word-quoted w)) (wildcard-p text))))
                  (or (glob-expand text) (list text)))
                 (t (list text)))))
