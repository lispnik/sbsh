;;;; parser.lisp --- Build pipelines and and-or lists from tokens.

(in-package #:sbsh)

(defstruct command
  (words '())        ; raw WORD structs (expanded at execution time)
  (redir-specs '())  ; raw (TYPE FD TARGET): TARGET is a WORD or, for :dup, an int
  (argv '())         ; filled in by REALIZE-COMMAND at execution time
  (redirs '())       ; filled in by REALIZE-COMMAND at execution time
  (lisp nil))        ; a Lisp form, when this stage is a `(...)` Lisp filter

(defstruct pipeline
  (commands '())       ; list of COMMAND
  (background nil))    ; T if terminated with &

(defstruct clause
  pipeline
  (connector :seq))    ; how this pipeline relates to the next: :seq :and :or

;;; A command line is split into clause *segments* by the top-level control
;;; operators (; && || &) FIRST, without any expansion.  Each segment is then
;;; tokenized and parsed only when it is about to run, so that $?, cwd, and
;;; environment changes made by earlier clauses are visible to later ones.

(defun strip-comment (string)
  "Remove a trailing # comment from STRING.  A # only starts a comment when it
is unquoted, outside parens (so Lisp #' and #( survive), and begins a word."
  (let ((i 0) (n (length string)) (q nil) (boundary t) (depth 0))
    (loop while (< i n) do
      (let ((c (char string i)))
        (cond
          (q (when (char= c q) (setf q nil)) (setf boundary nil) (incf i))
          ((char= c #\\) (setf boundary nil) (incf i 2))
          ((or (char= c #\') (char= c #\")) (setf q c) (setf boundary nil) (incf i))
          ((char= c #\() (incf depth) (setf boundary nil) (incf i))
          ((char= c #\)) (when (plusp depth) (decf depth)) (setf boundary nil) (incf i))
          ((and (char= c #\#) boundary (zerop depth))
           (return-from strip-comment (subseq string 0 i)))
          ((member c '(#\Space #\Tab #\; #\& #\|)) (setf boundary t) (incf i))
          (t (setf boundary nil) (incf i)))))
    string))

(defun split-clauses (raw-string)
  "Split STRING into a list of (:TEXT seg :TERMINATOR op) plists at top-level
control operators, honoring quotes.  OP is one of :SEMI :AMP :AND :OR or NIL."
  (let* ((string (strip-comment raw-string))
         (clauses '()) (start 0) (i 0) (n (length string)) (q nil) (depth 0))
    (flet ((emit (end term consumed)
             (push (list :text (subseq string start end) :terminator term) clauses)
             (setf start (+ end consumed))))
      (loop while (< i n) do
        (let ((c (char string i)))
          (cond
            (q (when (char= c q) (setf q nil)) (incf i))
            ((char= c #\\) (incf i 2))
            ((or (char= c #\') (char= c #\")) (setf q c) (incf i))
            ;; Inside ( ) — a Lisp form or $(...) — nothing is a separator.
            ((char= c #\() (incf depth) (incf i))
            ((char= c #\)) (when (plusp depth) (decf depth)) (incf i))
            ((plusp depth) (incf i))
            ((char= c #\;) (emit i :semi 1) (incf i))
            ((char= c #\&)
             (cond
               ((and (< (1+ i) n) (char= (char string (1+ i)) #\&))
                (emit i :and 2) (incf i 2))
               ;; A & that is part of a >& fd-duplication is NOT a separator.
               ((and (> i 0) (char= (char string (1- i)) #\>)) (incf i))
               (t (emit i :amp 1) (incf i))))
            ((and (char= c #\|) (< (1+ i) n) (char= (char string (1+ i)) #\|))
             (emit i :or 2) (incf i 2))
            (t (incf i)))))
      (emit n nil 0)
      (remove-if (lambda (cl)
                   (and (null (getf cl :terminator))
                        (string= "" (string-trim '(#\Space #\Tab #\Newline)
                                                 (getf cl :text)))))
                 (nreverse clauses)))))

(defun split-pipeline-stages (string)
  "Split a clause segment into raw stage strings at top-level | (respecting
quotes and parens, so Lisp forms and $(...) are not split)."
  (let ((stages '()) (start 0) (i 0) (n (length string)) (q nil) (depth 0))
    (loop while (< i n) do
      (let ((c (char string i)))
        (cond
          (q (when (char= c q) (setf q nil)) (incf i))
          ((char= c #\\) (incf i 2))
          ((or (char= c #\') (char= c #\")) (setf q c) (incf i))
          ((char= c #\() (incf depth) (incf i))
          ((char= c #\)) (when (plusp depth) (decf depth)) (incf i))
          ((and (zerop depth) (char= c #\|))
           (push (subseq string start i) stages) (setf start (1+ i)) (incf i))
          (t (incf i)))))
    (push (subseq string start) stages)
    (nreverse stages)))

(defun lisp-stage-p (string)
  (let ((s (string-left-trim '(#\Space #\Tab) string)))
    (and (plusp (length s)) (char= (char s 0) #\())))

(defun parse-stage (string)
  "Parse one pipeline stage: a Lisp `(...)` filter or an ordinary command."
  (if (lisp-stage-p string)
      (make-command :lisp (let ((*package* *user-package*))
                            (read-from-string string)))
      (let ((cmd (build-command (tokenize string))))
        (and (or (command-words cmd) (command-redir-specs cmd)) cmd))))

(defun parse-segment (string)
  "Parse a single clause segment (no control operators) into a PIPELINE,
splitting it into stages, or NIL when the segment is empty."
  (let ((commands (loop for s in (split-pipeline-stages string)
                        for cmd = (parse-stage s)
                        when cmd collect cmd)))
    (and commands (make-pipeline :commands commands))))

(defun terminator->connector (term)
  (case term (:and :and) (:or :or) (t :seq)))

(defun parse-line (string)
  "Parse a whole command line into fully-realized CLAUSE structures.
Used by the test-suite and for one-shot parsing; the interactive executor
parses each clause lazily via SPLIT-CLAUSES / PARSE-SEGMENT."
  (loop for cl in (split-clauses string)
        for pl = (parse-segment (getf cl :text))
        when pl
          collect (progn
                    (when (eq (getf cl :terminator) :amp)
                      (setf (pipeline-background pl) t))
                    (mapc #'realize-command (pipeline-commands pl))
                    (make-clause :pipeline pl
                                 :connector (terminator->connector
                                             (getf cl :terminator))))))

(defun build-command (tokens)
  "Build a COMMAND from a flat list of WORD structs and :REDIR tokens.
Words are kept raw; expansion is deferred to REALIZE-COMMAND at run time."
  (let ((words '())
        (redirs '()))
    (loop for rest = tokens then (cdr rest)
          while rest
          for tok = (car rest)
          do (cond
               ((word-p tok) (push tok words))
               ((and (consp tok) (eq (car tok) :redir))
                (destructuring-bind (redir type fd &optional target) tok
                  (declare (ignore redir))
                  (case type
                    (:dup (push (list :dup fd target) redirs))
                    (t (let ((fname (cadr rest)))
                         (unless (word-p fname)
                           (error 'shell-parse-error
                                  :message "expected filename after redirection"))
                         (setf rest (cdr rest))  ; consume the filename word
                         (push (list type fd fname) redirs))))))
               (t (error 'shell-parse-error :message "unexpected token"))))
    (make-command :words (nreverse words)
                  :redir-specs (nreverse redirs))))

(defun realize-command (cmd)
  "Perform expansion (variables were expanded during tokenizing; here we glob
and tilde-expand) just before CMD runs, filling in ARGV and REDIRS.  Returns
CMD.  This is done at execution time so that $?, cwd, and environment changes
from earlier commands on the same line are visible."
  (unless (command-lisp cmd)             ; Lisp filter stages are not expanded
    (setf (command-argv cmd) (expand-words (command-words cmd)))
    (apply-alias cmd)
    (setf (command-redirs cmd)
          (loop for (type fd target) in (command-redir-specs cmd)
                collect (if (eq type :dup)
                            (list :dup fd target)
                            (list type fd (redir-target target))))))
  cmd)

(defun apply-alias (cmd)
  "Expand a leading command-name alias in CMD's ARGV (one level, non-recursive)."
  (let ((argv (command-argv cmd)))
    (when (and argv (nth-value 1 (gethash (first argv) *aliases*)))
      (setf (command-argv cmd)
            (append (gethash (first argv) *aliases*) (rest argv))))))

(defun redir-target (word)
  "Resolve a redirection filename WORD (tilde/glob expansion, first match)."
  (expand-tilde (first (expand-words (list word)))))
