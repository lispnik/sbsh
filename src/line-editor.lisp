;;;; line-editor.lisp --- A raw-mode readline: cursor movement, history
;;;; navigation, incremental search, and filename completion.

(in-package #:sbsh)

(defvar *tty-in* nil)
(defvar *tty-out* nil)

(defun tty-in ()
  (or *tty-in*
      (setf *tty-in* (sb-sys:make-fd-stream 0 :input t
                                              :external-format :utf-8
                                              :buffering :none))))
(defun tty-out ()
  (or *tty-out*
      (setf *tty-out* (sb-sys:make-fd-stream 1 :output t
                                               :external-format :utf-8
                                               :buffering :full))))

(defun out (fmt &rest args)
  (apply #'format (tty-out) fmt args))

(defun flush () (force-output (tty-out)))

;;; --- Editor state -------------------------------------------------------

(defstruct (led (:constructor make-led (prompt)))
  (buf (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
  (point 0)
  prompt
  (hist-idx nil)   ; index into *history* while browsing, else NIL
  (saved ""))      ; the fresh line stashed while browsing history

(defun ed-len (ed) (fill-pointer (led-buf ed)))

(defun ed-text (ed) (coerce (led-buf ed) 'simple-string))

(defun ed-set-text (ed string)
  (let ((buf (led-buf ed)))
    (setf (fill-pointer buf) 0)
    (loop for c across string do (vector-push-extend c buf))
    (setf (led-point ed) (length string))))

(defun ed-insert (ed char)
  (let ((buf (led-buf ed)) (p (led-point ed)))
    (vector-push-extend #\Nul buf)           ; grow by one
    (loop for i from (1- (fill-pointer buf)) above p
          do (setf (aref buf i) (aref buf (1- i))))
    (setf (aref buf p) char)
    (incf (led-point ed))))

(defun ed-delete-back (ed)
  (let ((buf (led-buf ed)) (p (led-point ed)))
    (when (> p 0)
      (loop for i from (1- p) below (1- (fill-pointer buf))
            do (setf (aref buf i) (aref buf (1+ i))))
      (decf (fill-pointer buf))
      (decf (led-point ed)))))

(defun ed-delete-forward (ed)
  (let ((buf (led-buf ed)) (p (led-point ed)))
    (when (< p (fill-pointer buf))
      (loop for i from p below (1- (fill-pointer buf))
            do (setf (aref buf i) (aref buf (1+ i))))
      (decf (fill-pointer buf)))))

(defun ed-kill-to-end (ed)
  (setf (fill-pointer (led-buf ed)) (led-point ed)))

(defun ed-kill-to-start (ed)
  (let ((buf (led-buf ed)) (p (led-point ed)))
    (loop for i from 0 below (- (fill-pointer buf) p)
          do (setf (aref buf i) (aref buf (+ i p))))
    (setf (fill-pointer buf) (- (fill-pointer buf) p))
    (setf (led-point ed) 0)))

(defun ed-delete-word-back (ed)
  (let ((p (led-point ed)) (buf (led-buf ed)))
    (loop while (and (> p 0) (member (aref buf (1- p)) '(#\Space #\Tab)))
          do (decf p))
    (loop while (and (> p 0) (not (member (aref buf (1- p)) '(#\Space #\Tab))))
          do (decf p))
    (loop repeat (- (led-point ed) p) do (ed-delete-back ed))))

;;; --- Rendering (linenoise-style single-line refresh) --------------------

(defun visible-length (string)
  "Length of STRING as displayed, ignoring ANSI CSI escape sequences."
  (let ((n 0) (i 0) (len (length string)))
    (loop while (< i len) do
      (if (and (char= (char string i) #\Escape)
               (< (1+ i) len) (char= (char string (1+ i)) #\[))
          (progn (incf i 2)
                 (loop while (and (< i len)
                                  (not (char<= #\@ (char string i) #\~)))
                       do (incf i))
                 (incf i))
          (progn (incf n) (incf i))))
    n))

(defun refresh-line (ed)
  "Redraw the prompt and buffer on the current terminal row, scrolling the
buffer horizontally when it would not otherwise fit."
  (let* ((cols (max 1 (terminal-columns)))
         (prompt (led-prompt ed))
         (plen (visible-length prompt))
         (text (ed-text ed))
         (len (length text))
         (pos (led-point ed))
         (start 0))
    ;; Scroll left until the cursor fits.
    (loop while (>= (+ plen (- pos start)) cols) do (incf start))
    ;; Trim the right edge to the terminal width.
    (let ((end len))
      (loop while (> (+ plen (- end start)) cols) do (decf end))
      (let ((cursor-col (+ plen (- pos start))))
        (out "~C[0G" #\Escape)             ; cursor to column 0
        (write-string prompt (tty-out))
        (write-string text (tty-out) :start start :end end)
        (out "~C[0K" #\Escape)             ; erase to end of line
        (out "~C[~DG" #\Escape (1+ cursor-col)) ; cursor to column
        (flush)))))

;;; --- Key input ----------------------------------------------------------

(defun read-key ()
  "Read one logical key from the terminal, decoding escape sequences into
keyword symbols (:up :down :left :right :home :end :delete) or returning a
character / :eof."
  (let ((c (read-char (tty-in) nil :eof)))
    (cond
      ((eq c :eof) :eof)
      ((char/= c #\Escape) c)
      ;; A bare ESC with nothing following it.
      ((not (listen (tty-in))) :escape)
      (t (let ((c2 (read-char (tty-in) nil :eof)))
           (cond
             ((member c2 '(#\[ #\O))
              (let ((c3 (read-char (tty-in) nil :eof)))
                (case c3
                  (#\A :up) (#\B :down) (#\C :right) (#\D :left)
                  (#\H :home) (#\F :end)
                  (t (if (and (characterp c3) (digit-char-p c3))
                         ;; e.g. ESC [ 3 ~  (extended keys)
                         (let ((digits (list c3)))
                           (loop for c4 = (read-char (tty-in) nil :eof)
                                 until (or (eq c4 :eof) (char= c4 #\~))
                                 do (push c4 digits))
                           (let ((code (parse-integer
                                        (coerce (nreverse digits) 'string)
                                        :junk-allowed t)))
                             (case code
                               (3 :delete)
                               ((1 7) :home)
                               ((4 8) :end)
                               (t :unknown))))
                         :unknown)))))
             (t :unknown)))))))

;;; --- Filename completion ------------------------------------------------

(defun current-token-bounds (text point)
  "Return (values START END) of the whitespace-delimited token at POINT."
  (let ((start point))
    (loop while (and (> start 0)
                     (not (member (char text (1- start)) '(#\Space #\Tab))))
          do (decf start))
    (values start point)))

(defun completion-candidates (token)
  "Return a list of filesystem completions for the partial path TOKEN."
  (let* ((expanded (expand-tilde token))
         (slash (position #\/ expanded :from-end t))
         (dir (if slash (subseq expanded 0 (1+ slash)) "./"))
         (base (if slash (subseq expanded (1+ slash)) expanded))
         (dirpath (ignore-errors (truename dir))))
    (when dirpath
      (let (names)
        (dolist (p (ignore-errors (uiop:directory-files dirpath)))
          (let ((n (file-namestring p)))
            (when (and (plusp (length n)) (starts-with-subseq base n))
              (push n names))))
        (dolist (p (ignore-errors (uiop:subdirectories dirpath)))
          (let ((n (car (last (pathname-directory p)))))
            (when (and (stringp n) (starts-with-subseq base n))
              (push (concatenate 'string n "/") names))))
        (values (sort names #'string<) dir base)))))

(defun longest-common-prefix (strings)
  (if (null strings)
      ""
      (reduce (lambda (a b)
                (subseq a 0 (or (mismatch a b) (length a))))
              strings)))

(defun complete-token (ed)
  "Attempt filename completion of the token under the cursor."
  (let* ((text (ed-text ed)) (point (led-point ed)))
    (multiple-value-bind (start end) (current-token-bounds text point)
      (let ((token (subseq text start end)))
        (multiple-value-bind (names dir base) (completion-candidates token)
          (declare (ignore base))
          (when names
            (let ((prefix (longest-common-prefix names)))
              (cond
                ((= (length names) 1)
                 (replace-token ed start end (concatenate 'string dir (first names))))
                ((> (length prefix) 0)
                 (replace-token ed start end (concatenate 'string dir prefix))
                 (when (> (length names) 1)
                   ;; Show the choices, then repaint the prompt line.
                   (out "~%")
                   (out "~{~A~^  ~}~%" names)
                   (refresh-line ed)))
                (t (refresh-line ed))))))))))

(defun replace-token (ed start end new-text)
  (let ((text (ed-text ed)))
    (ed-set-text ed (concatenate 'string
                                 (subseq text 0 start)
                                 new-text
                                 (subseq text end)))
    (setf (led-point ed) (+ start (length new-text)))
    (refresh-line ed)))

;;; --- History browsing ---------------------------------------------------

(defun history-prev (ed)
  (when (plusp (history-count))
    (when (null (led-hist-idx ed))
      (setf (led-saved ed) (ed-text ed)))
    (let ((idx (if (null (led-hist-idx ed))
                   (1- (history-count))
                   (max 0 (1- (led-hist-idx ed))))))
      (setf (led-hist-idx ed) idx)
      (ed-set-text ed (history-ref idx))
      (refresh-line ed))))

(defun history-next (ed)
  (when (led-hist-idx ed)
    (let ((idx (1+ (led-hist-idx ed))))
      (if (>= idx (history-count))
          (progn (setf (led-hist-idx ed) nil)
                 (ed-set-text ed (led-saved ed)))
          (progn (setf (led-hist-idx ed) idx)
                 (ed-set-text ed (history-ref idx))))
      (refresh-line ed))))

;;; --- Reverse incremental search (C-r) -----------------------------------

(defun reverse-search (ed)
  "Interactive reverse-i-search.  On accept, the found line replaces the
editor buffer; ESC or C-g cancels."
  (let ((query (make-array 0 :element-type 'character
                             :adjustable t :fill-pointer 0))
        (from (1- (history-count)))
        (match nil))
    (labels ((show ()
               (out "~C[0G~C[0K(reverse-i-search)`~A': ~A"
                    #\Escape #\Escape (coerce query 'string) (or match ""))
               (flush))
             (do-search ()
               (let ((idx (history-search-backward (coerce query 'string) from)))
                 (setf match (and idx (history-ref idx)))
                 (when idx (setf from idx)))))
      (do-search) (show)
      (loop
        (let ((k (read-key)))
          (cond
            ((eq k :eof) (return))
            ((eql k #\Rubout) (when (plusp (fill-pointer query))
                                (decf (fill-pointer query))
                                (setf from (1- (history-count)))
                                (do-search) (show)))
            ((eql k (code-char 18))       ; C-r again: search older
             (setf from (max 0 (1- from))) (do-search) (show))
            ((or (eql k :escape) (eql k (code-char 7))) ; ESC / C-g: cancel
             (return))
            ((or (eql k #\Return) (eql k #\Newline)) ; accept
             (when match (ed-set-text ed match))
             (return))
            ((and (characterp k) (graphic-char-p k))
             (vector-push-extend k query)
             (setf from (1- (history-count)))
             (do-search) (show))
            (t (when match (ed-set-text ed match)) (return)))))
      (refresh-line ed))))

;;; --- Main entry ---------------------------------------------------------

(defun read-line-interactive (prompt)
  "Read a line of input with full editing.  Returns the string, :EOF on an
end-of-input on an empty line, or :CANCEL when the line is aborted (C-c)."
  (let ((ed (make-led prompt)))
    (with-raw-mode (*shell-terminal*)
      (refresh-line ed)
      (loop
        (let ((k (read-key)))
          (cond
            ((eq k :eof)
             (if (zerop (ed-len ed))
                 (progn (out "~%") (flush) (return :eof))
                 (progn (ed-delete-forward ed) (refresh-line ed))))
            ((or (eql k #\Return) (eql k #\Newline))
             (out "~%") (flush) (return (ed-text ed)))
            ((eql k (code-char 3))        ; C-c
             (out "^C~%") (flush) (return :cancel))
            ((eql k (code-char 4))        ; C-d
             (if (zerop (ed-len ed))
                 (progn (out "~%") (flush) (return :eof))
                 (progn (ed-delete-forward ed) (refresh-line ed))))
            ((or (eql k #\Rubout) (eql k (code-char 8))) ; backspace
             (ed-delete-back ed) (refresh-line ed))
            ((eql k (code-char 1)) (setf (led-point ed) 0) (refresh-line ed))       ; C-a
            ((eql k (code-char 5)) (setf (led-point ed) (ed-len ed)) (refresh-line ed)) ; C-e
            ((eql k (code-char 2)) (when (> (led-point ed) 0) (decf (led-point ed))) (refresh-line ed)) ; C-b
            ((eql k (code-char 6)) (when (< (led-point ed) (ed-len ed)) (incf (led-point ed))) (refresh-line ed)) ; C-f
            ((eql k (code-char 11)) (ed-kill-to-end ed) (refresh-line ed))          ; C-k
            ((eql k (code-char 21)) (ed-kill-to-start ed) (refresh-line ed))        ; C-u
            ((eql k (code-char 23)) (ed-delete-word-back ed) (refresh-line ed))     ; C-w
            ((eql k (code-char 12))                                                 ; C-l
             (out "~C[H~C[2J" #\Escape #\Escape) (refresh-line ed))
            ((eql k (code-char 16)) (history-prev ed))   ; C-p
            ((eql k (code-char 14)) (history-next ed))   ; C-n
            ((eql k (code-char 18)) (reverse-search ed)) ; C-r
            ((eql k #\Tab) (complete-token ed))
            ((eq k :up) (history-prev ed))
            ((eq k :down) (history-next ed))
            ((eq k :left) (when (> (led-point ed) 0) (decf (led-point ed))) (refresh-line ed))
            ((eq k :right) (when (< (led-point ed) (ed-len ed)) (incf (led-point ed))) (refresh-line ed))
            ((eq k :home) (setf (led-point ed) 0) (refresh-line ed))
            ((eq k :end) (setf (led-point ed) (ed-len ed)) (refresh-line ed))
            ((eq k :delete) (ed-delete-forward ed) (refresh-line ed))
            ((member k '(:escape :unknown)) nil)
            ((and (characterp k) (or (graphic-char-p k) (char>= k #\Space)))
             (ed-insert ed k) (refresh-line ed))
            (t nil)))))))
