;;;; lexer.lisp --- Tokenizer with quoting, expansion, and globbing.

(in-package #:sbsh)

;;; A word token carries its expanded text plus whether any part was quoted
;;; (quoting suppresses later globbing).  Operators are represented as
;;; keywords, and redirections as (:REDIR type fd [target]) lists.

(defstruct (word (:constructor make-word (text &optional quoted)))
  text
  (quoted nil))

(define-condition shell-parse-error (error)
  ((message :initarg :message :reader parse-error-message))
  (:report (lambda (c s) (format s "~A" (parse-error-message c)))))

;;; --- Environment / expansion --------------------------------------------

(defun getenv (name) (sb-posix:getenv name))

(defun expand-tilde (string)
  "Expand a leading ~ (or ~/...) to the user's home directory."
  (cond
    ((string= string "~")
     (namestring (user-homedir-pathname)))
    ((and (> (length string) 1) (char= (char string 0) #\~)
          (char= (char string 1) #\/))
     (concatenate 'string
                  (string-right-trim "/" (namestring (user-homedir-pathname)))
                  (subseq string 1)))
    (t string)))

(defun var-value (name)
  "Look up a variable NAME: specials ($? $$ $# $@ $* $0), positional params
($1..), then the environment."
  (cond
    ((string= name "?") (princ-to-string *last-status*))
    ((string= name "$") (princ-to-string (sb-posix:getpid)))
    ((string= name "#") (princ-to-string (length *positional*)))
    ((or (string= name "@") (string= name "*"))
     (format nil "~{~A~^ ~}" *positional*))
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
            (error 'shell-error :message (format nil "~A: unbound variable" name)))
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

(defun substring-of (val spec)
  "Return the ${var:offset[:length]} substring of VAL; SPEC is the text after
the colon."
  (let* ((c (position #\: spec))
         (off (or (parse-integer (if c (subseq spec 0 c) spec) :junk-allowed t) 0))
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
    ;; ${#name} length (but not ${#} which is above, nor ${#var...ops})
    ((and (char= (char content 0) #\#) (> (length content) 1)
          (let ((c1 (char content 1))) (or (var-name-char-p c1) (member c1 '(#\@ #\*)))))
     (princ-to-string (length (var-value (subseq content 1)))))
    (t (let* ((ne (param-name-end content))
              (name (subseq content 0 ne))
              (rest (subseq content ne))
              (val (var-value name)))
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

(defun apply-default (val name op word colon)
  "Handle the ${var OP word} default/alternate operators."
  (let ((missing (if colon (zerop (length val)) (null (getenv name)))))
    (flet ((w () (expand-heredoc-body word)))
      (case op
        (#\- (if missing (w) val))
        (#\= (if missing (let ((v (w))) (sb-posix:setenv name v 1) v) val))
        (#\+ (if missing "" (w)))
        (#\? (if missing
                 (error 'shell-error :message
                        (format nil "~A: ~A" name
                                (if (plusp (length word)) (w) "parameter null or not set")))
                 val))
        (t val)))))

;;; --- Globbing -----------------------------------------------------------

(defun wildcard-p (s)
  (or (find #\* s) (find #\? s) (find #\[ s)))

(defun fnmatch (pattern name)
  "Match a single path segment NAME against PATTERN (* ? and [..] classes)."
  (labels ((m (px nx)
             (cond
               ((= px (length pattern)) (= nx (length name)))
               ((char= (char pattern px) #\*)
                (or (m (1+ px) nx)
                    (and (< nx (length name)) (m px (1+ nx)))))
               ((= nx (length name)) nil)
               ((char= (char pattern px) #\?) (m (1+ px) (1+ nx)))
               ((char= (char pattern px) #\[)
                (multiple-value-bind (ok next) (match-class pattern px (char name nx))
                  (and ok (m next (1+ nx)))))
               ((char= (char pattern px) (char name nx)) (m (1+ px) (1+ nx)))
               (t nil))))
    (m 0 0)))

(defun match-class (pattern px ch)
  "Match character CH against a [..] class starting at PX in PATTERN.
Returns (values MATCHED-P INDEX-AFTER-CLASS)."
  (let* ((i (1+ px))
         (negate (and (< i (length pattern))
                      (member (char pattern i) '(#\! #\^)))))
    (when negate (incf i))
    (let ((matched nil) (start i))
      (loop
        (when (>= i (length pattern)) (return))
        (when (and (char= (char pattern i) #\]) (> i start)) (return))
        (if (and (< (+ i 2) (length pattern))
                 (char= (char pattern (1+ i)) #\-)
                 (char/= (char pattern (+ i 2)) #\]))
            (progn
              (when (char<= (char pattern i) ch (char pattern (+ i 2)))
                (setf matched t))
              (incf i 3))
            (progn
              (when (char= (char pattern i) ch) (setf matched t))
              (incf i))))
      (values (if negate (not matched) matched) (1+ i)))))

(defun list-dir-entries (dir)
  "Return a list of (NAME . DIRECTORY-P) for the entries of DIR (\"\" = cwd)."
  (let ((path (if (string= dir "")
                  *default-pathname-defaults*
                  (pathname (if (char= (char dir (1- (length dir))) #\/)
                                dir (concatenate 'string dir "/")))))
        (entries '()))
    (dolist (f (ignore-errors (uiop:directory-files path)))
      (let ((n (file-namestring f)))
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
             (if (member next '(#\" #\\ #\$ #\`))
                 (progn (write-char next out) (incf i 2))
                 (progn (write-char c out) (incf i)))))
          ((char= c #\$)
           (multiple-value-bind (val ni) (read-variable string (1+ i))
             (write-string val out) (setf i ni)))
          (t (write-char c out) (incf i)))))))

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

(defun read-variable (string i)
  "Read a $NAME, ${NAME}, or $(...) reference starting after the $ at I.
Returns (values VALUE INDEX-AFTER)."
  (let ((n (length string)))
    (cond
      ((>= i n) (values "$" i))
      ((char= (char string i) #\()
       (multiple-value-bind (body end) (read-balanced-parens string i)
         (values (command-substitute body) end)))
      ((char= (char string i) #\{)
       (let ((end (position #\} string :start i)))
         (unless end (error 'shell-parse-error :message "unterminated ${"))
         (values (braced-var-value (subseq string (1+ i) end)) (1+ end))))
      ;; $? $$ $# $@ $* and single-digit positionals $1..$9
      ((member (char string i) '(#\? #\$ #\# #\@ #\* #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
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
      (t (let ((end (or (position-if
                         (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return
                                                 #\< #\> #\| #\& #\; #\( #\))))
                         string :start j)
                        n)))
           (values (subseq string j end) nil end))))))

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
        (quoted nil))
    (labels ((ensure-cur () (unless cur (setf cur (make-string-output-stream))))
             ;; Non-destructively read the current word's text so the fd-digit
             ;; check in < / > does not consume it (echo a>file).
             (pending () (when cur
                           (let ((s (get-output-stream-string cur)))
                             (write-string s cur) s)))
             (flush ()
               (when cur
                 (push (make-word (get-output-stream-string cur) quoted) tokens)
                 (setf cur nil quoted nil)))
             (peek (k) (and (< (+ i k) n) (char string (+ i k)))))
      (loop
        (when (>= i n) (return))
        (let ((c (char string i)))
          (cond
            ((member c '(#\Space #\Tab #\Newline #\Return)) (flush) (incf i))
            ;; "$@" -> one word per positional param; "$*" -> a single joined word.
            ((and (char= c #\") (< (+ i 3) n)
                  (char= (char string (1+ i)) #\$)
                  (member (char string (+ i 2)) '(#\@ #\*))
                  (char= (char string (+ i 3)) #\"))
             (flush)
             (if (char= (char string (+ i 2)) #\*)
                 (push (make-word (format nil "~{~A~^ ~}" *positional*) t) tokens)
                 (dolist (p *positional*) (push (make-word p t) tokens)))
             (incf i 4))
            ((char= c #\')
             (ensure-cur)
             (multiple-value-bind (text ni) (read-single-quoted string (1+ i))
               (write-string text cur) (setf quoted t i ni)))
            ((char= c #\")
             (ensure-cur)
             (multiple-value-bind (text ni) (read-double-quoted string (1+ i))
               (write-string text cur) (setf quoted t i ni)))
            ((char= c #\\)
             (ensure-cur)
             (when (< (1+ i) n) (write-char (char string (1+ i)) cur) (setf quoted t))
             (incf i 2))
            ;; Unquoted $@ / $* -> each positional as a separate word.
            ((and (char= c #\$) (< (1+ i) n) (member (char string (1+ i)) '(#\@ #\*)))
             (flush)
             (dolist (p *positional*) (push (make-word p) tokens))
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
                         (t (write-string (first fields) cur)
                            (dolist (f (rest fields))
                              (flush) (ensure-cur) (write-string f cur)))))))))
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
               (when (and cur fd) (setf cur nil quoted nil))
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
                 (t (push (list :redir :in (or fd 0)) tokens) (incf i)))))
            ((char= c #\>)
             (let ((fd (digits-or nil (pending))))
               (when (and cur fd) (setf cur nil quoted nil))
               (flush)
               (cond
                 ((eql (peek 1) #\>) (push (list :redir :append (or fd 1)) tokens) (incf i 2))
                 ((and (eql (peek 1) #\&) (peek 2) (digit-char-p (peek 2)))
                  (push (list :redir :dup (or fd 1) (digit-char-p (peek 2))) tokens)
                  (incf i 3))
                 (t (push (list :redir :out (or fd 1)) tokens) (incf i)))))
            (t (ensure-cur) (write-char c cur) (incf i)))))
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
                 ((word-quoted w) (list text))
                 ((zerop (length text)) nil)   ; unquoted empty expansion: no word
                 ((wildcard-p text) (or (glob-expand text) (list text)))
                 (t (list text)))))
