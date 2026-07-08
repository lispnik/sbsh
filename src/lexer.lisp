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
  "Look up shell/environment variable NAME, handling specials $? and $$."
  (cond
    ((string= name "?") (princ-to-string *last-status*))
    ((string= name "$") (princ-to-string (sb-posix:getpid)))
    (t (or (getenv name) ""))))

(defun var-name-char-p (c)
  (or (alphanumericp c) (char= c #\_)))

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
         (values (var-value (subseq string (1+ i) end)) (1+ end))))
      ((or (char= (char string i) #\?) (char= (char string i) #\$))
       (values (var-value (string (char string i))) (1+ i)))
      ((var-name-char-p (char string i))
       (let ((end (or (position-if-not #'var-name-char-p string :start i) n)))
         (values (var-value (subseq string i end)) end)))
      (t (values "$" i)))))

(defun tokenize (string)
  "Split STRING into a list of WORD structs and operator/redirection tokens.
Performs quote removal and $/~ expansion; globbing is deferred to EXPAND-WORDS."
  (let ((tokens '())
        (i 0)
        (n (length string))
        (cur nil)               ; string-output-stream for the current word
        (quoted nil))
    (labels ((ensure-cur () (unless cur (setf cur (make-string-output-stream))))
             (pending () (and cur (get-output-stream-string cur)))
             (flush ()
               (when cur
                 (push (make-word (get-output-stream-string cur) quoted) tokens)
                 (setf cur nil quoted nil)))
             (peek (k) (and (< (+ i k) n) (char string (+ i k)))))
      (loop
        (when (>= i n) (return))
        (let ((c (char string i)))
          (cond
            ((member c '(#\Space #\Tab)) (flush) (incf i))
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
            ((char= c #\$)
             (ensure-cur)
             (multiple-value-bind (val ni) (read-variable string (1+ i))
               (write-string val cur) (setf i ni)))
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
               (push (list :redir :in (or fd 0)) tokens) (incf i)))
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
flat list of strings.  Quoted words are passed through literally."
  (loop for w in words
        for text = (if (word-quoted w) (word-text w) (maybe-tilde (word-text w)))
        append (if (and (not (word-quoted w)) (wildcard-p text))
                   (or (glob-expand text) (list text))
                   (list text))))
