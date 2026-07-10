;;;; grammar.lisp --- Control-flow compound commands: if, while/until, for,
;;;; case, and break/continue.
;;;;
;;;; A compound is stored by the parser as its raw source text; here we parse
;;;; the keyword structure and interpret it, running the constituent lists via
;;;; RUN-COMMAND-LINE so that expansions happen freshly each iteration.

(in-package #:sbsh)

;;; --- Keyword scanning ---------------------------------------------------

(defun scan-to-keyword (text start keywords)
  "From START, return (values MATCHED-WORD KW-START KW-END) for the next
top-level (not nested in a deeper compound, unquoted, command-position)
occurrence of a word in KEYWORDS; NIL if none before the end."
  (let ((i start) (n (length text)) (q nil) (cmdpos t) (depth 0))
    (loop while (< i n) do
      (let ((c (char text i)))
        (cond
          (q (when (char= c q) (setf q nil)) (incf i))
          ((char= c #\\) (incf i 2))
          ((or (char= c #\') (char= c #\")) (setf q c) (incf i) (setf cmdpos nil))
          ((member c '(#\Space #\Tab)) (incf i))
          ((member c '(#\Newline #\; #\& #\| #\()) (incf i) (setf cmdpos t))
          ((char= c #\)) (incf i) (setf cmdpos nil))
          ((not (structural-char-p c))
           ;; Read a whole word (even when not in command position, so `in`
           ;; after the case/for word is seen); classify openers/closers only
           ;; in command position; `in` matches after a word too.
           (multiple-value-bind (word nexti) (read-bareword text i)
             (let ((cls (classify-compound-word word)))
               (cond
                 ((and (zerop depth) (member word keywords :test #'string=)
                       (or cmdpos (string= word "in")))
                  (return-from scan-to-keyword (values word i nexti)))
                 ((and cmdpos (eq cls :open)) (incf depth))
                 ((and cmdpos (eq cls :close)) (when (plusp depth) (decf depth))))
               (setf i nexti)
               (setf cmdpos (and (member word *compound-continue-words* :test #'string=) t)))))
          (t (incf i) (setf cmdpos nil)))))
    nil))

(defun keyword-at (text pos word)
  "Skip WORD (and following blanks) at POS, returning the index after it."
  (let ((i (skip-blanks text pos)))
    (multiple-value-bind (w end) (read-bareword text i)
      (declare (ignore w))
      (skip-blanks text end))))

;;; --- if / elif / else ---------------------------------------------------

(defun eval-compound (text)
  "Parse and evaluate a compound command from its source TEXT."
  (let ((s (string-left-trim '(#\Space #\Tab #\Newline #\Return) text)))
    (multiple-value-bind (kw) (read-bareword s 0)
      (cond
        ((string= kw "if") (eval-if s))
        ((string= kw "while") (eval-while s nil))
        ((string= kw "until") (eval-while s t))
        ((string= kw "for") (eval-for s))
        ((string= kw "case") (eval-case s))
        (t (setf *last-status* 0))))))

(defun eval-if (text)
  "Evaluate `if COND; then BODY; [elif ...] [else ...] fi`."
  (let ((pos (keyword-at text 0 "if")) (ran nil))
    (block done
      (loop
        (multiple-value-bind (kw ks ke) (scan-to-keyword text pos '("then"))
          (declare (ignore kw ke))
          (unless ks (return-from done))
          (let ((cond-text (subseq text pos ks)))
            (multiple-value-bind (nkw bs be) (scan-to-keyword text ks '("elif" "else" "fi"))
              (declare (ignore be))
              (let* ((then-start (keyword-at text ks "then"))
                     (body-text (subseq text then-start (or bs (length text)))))
                (let ((*condition-context* t)) (run-command-line cond-text))
                (when (zerop *last-status*)
                  (setf ran t)
                  (run-command-line body-text)
                  (return-from done))
                (cond
                  ((null nkw) (return-from done))
                  ((string= nkw "elif") (setf pos (keyword-at text bs "elif")))
                  ((string= nkw "else")
                   (let* ((else-start (keyword-at text bs "else"))
                          (fi-pos (or (nth-value 1 (scan-to-keyword text else-start '("fi")))
                                      (length text))))
                     (setf ran t)
                     (run-command-line (subseq text else-start fi-pos))
                     (return-from done)))
                  ((string= nkw "fi") (return-from done)))))))))
    (unless ran (setf *last-status* 0))
    *last-status*))

;;; --- while / until ------------------------------------------------------

(defun eval-while (text until)
  "Evaluate `while COND; do BODY; done` (or `until`)."
  (let* ((pos (keyword-at text 0 (if until "until" "while"))))
    (multiple-value-bind (dkw ds de) (scan-to-keyword text pos '("do"))
      (declare (ignore dkw de))
      (unless ds (return-from eval-while (setf *last-status* 0)))
      (let* ((cond-text (subseq text pos ds))
             (body-start (keyword-at text ds "do"))
             (done-start (or (nth-value 1 (scan-to-keyword text body-start '("done")))
                             (length text)))
             (body-text (subseq text body-start done-start))
             (*loop-depth* (1+ *loop-depth*)))
        (catch 'sbsh-break
          (loop
            (let ((*condition-context* t)) (run-command-line cond-text))
            (let ((ok (zerop *last-status*)))
              (when until (setf ok (not ok)))
              (unless ok (return)))
            (catch 'sbsh-continue (run-command-line body-text))))
        *last-status*))))

;;; --- for ----------------------------------------------------------------

(defun eval-for (text)
  "Evaluate `for NAME [in WORDS]; do BODY; done`."
  (let* ((after-for (keyword-at text 0 "for"))
         (name-end (nth-value 1 (read-bareword text after-for)))
         (name (subseq text after-for name-end)))
    (multiple-value-bind (dkw ds de) (scan-to-keyword text name-end '("do"))
      (declare (ignore dkw de))
      (unless ds (return-from eval-for (setf *last-status* 0)))
      (multiple-value-bind (ikw is ie) (scan-to-keyword text name-end '("in"))
        (declare (ignore ikw))
        (let* ((words-text (if (and is (< is ds)) (subseq text ie ds) nil))
               (items (if words-text
                          (expand-words (remove-if-not #'word-p (tokenize words-text)))
                          (copy-list *positional*)))
               (body-start (keyword-at text ds "do"))
               (done-start (or (nth-value 1 (scan-to-keyword text body-start '("done")))
                               (length text)))
               (body-text (subseq text body-start done-start))
               (*loop-depth* (1+ *loop-depth*)))
          (setf *last-status* 0)
          (catch 'sbsh-break
            (dolist (item items)
              (sb-posix:setenv name item 1)
              (catch 'sbsh-continue (run-command-line body-text))))
          *last-status*)))))

;;; --- case ---------------------------------------------------------------

(defun eval-case (text)
  "Evaluate `case WORD in PATTERN) BODY ;; ... esac`."
  (let* ((after-case (keyword-at text 0 "case")))
    (multiple-value-bind (ikw is ie) (scan-to-keyword text after-case '("in"))
      (declare (ignore ikw))
      (unless is (return-from eval-case (setf *last-status* 0)))
      (let* ((word-text (subseq text after-case is))
             (word (or (first (expand-words (remove-if-not #'word-p (tokenize word-text)))) ""))
             (esac-pos (or (nth-value 1 (scan-to-keyword text ie '("esac"))) (length text)))
             (body (subseq text ie esac-pos)))
        (setf *last-status* 0)
        (block matched
          (dolist (clause (split-case-clauses body))
            (destructuring-bind (patterns . clause-body) clause
              (when (some (lambda (p) (fnmatch (expand-case-pattern p) word)) patterns)
                (run-command-line clause-body)
                (return-from matched)))))
        *last-status*))))

(defun expand-case-pattern (p)
  "Expand $vars in a case pattern but keep glob metacharacters (* ? [ ])
literal, since the pattern is matched with FNMATCH rather than globbed."
  (let* ((p (string-trim '(#\Space #\Tab #\Newline #\Return) p))
         (words (remove-if-not #'word-p (tokenize p))))
    (if words (word-text (first words)) p)))

(defun split-case-clauses (body)
  "Split a case body into a list of (PATTERNS . BODY-TEXT), where PATTERNS is a
list of pattern strings.  Clauses are `pat|pat) list ;;`."
  (let ((clauses '()) (i 0) (n (length body)))
    (loop
      (setf i (skip-blanks body i))
      (when (>= i n) (return))
      ;; optional leading ( before the pattern
      (when (and (< i n) (char= (char body i) #\()) (incf i))
      ;; read up to the unquoted )
      (let ((pat-start i) (q nil))
        (loop while (< i n) do
          (let ((c (char body i)))
            (cond
              (q (when (char= c q) (setf q nil)) (incf i))
              ((or (char= c #\') (char= c #\")) (setf q c) (incf i))
              ((char= c #\)) (return))
              (t (incf i)))))
        (when (>= i n) (return))
        (let ((patterns (mapcar (lambda (s) (string-trim '(#\Space #\Tab) s))
                                (split-on-char (subseq body pat-start i) #\|))))
          (incf i)                      ; skip )
          ;; body up to ;;
          (let ((body-start i) (bq nil))
            (loop while (< i n) do
              (let ((c (char body i)))
                (cond
                  (bq (when (char= c bq) (setf bq nil)) (incf i))
                  ((or (char= c #\') (char= c #\")) (setf bq c) (incf i))
                  ((and (char= c #\;) (< (1+ i) n) (char= (char body (1+ i)) #\;)) (return))
                  (t (incf i)))))
            (push (cons patterns (subseq body body-start i)) clauses)
            (when (< i n) (incf i 2))))))   ; skip ;;
    (nreverse clauses)))

;;; --- break / continue ---------------------------------------------------

(define-builtin "break" (args)
  (declare (ignore args))
  (if (plusp *loop-depth*)
      (throw 'sbsh-break 0)
      (progn (format *error-output* "break: only meaningful in a loop~%") 0)))

(define-builtin "continue" (args)
  (declare (ignore args))
  (if (plusp *loop-depth*)
      (throw 'sbsh-continue 0)
      (progn (format *error-output* "continue: only meaningful in a loop~%") 0)))
