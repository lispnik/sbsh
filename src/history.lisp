;;;; history.lisp --- Command history: storage, persistence, dedup.

(in-package #:sbsh)

(defvar *history* (make-array 0 :adjustable t :fill-pointer 0)
  "In-memory command history, oldest first.")

(defvar *history-max* 2000
  "Maximum number of entries kept in memory and on disk.")

(defvar *history-persist* t
  "When NIL, history changes are not written to disk (used by tests).")

(defun history-file ()
  "Pathname of the on-disk history file (~/.sbsh_history)."
  (merge-pathnames ".sbsh_history" (user-homedir-pathname)))

(defun history-count ()
  (fill-pointer *history*))

(defun history-ref (i)
  "Return history entry I (0-based, oldest first), or NIL if out of range."
  (when (and (>= i 0) (< i (history-count)))
    (aref *history* i)))

(defun history-trim ()
  "Drop the oldest entries so history stays within *HISTORY-MAX*."
  (let ((over (- (history-count) *history-max*)))
    (when (plusp over)
      (let ((kept (subseq *history* over)))
        (setf (fill-pointer *history*) 0)
        (loop for e across kept do (vector-push-extend e *history*))))))

(defun history-add (line)
  "Add LINE to history, ignoring blanks and consecutive duplicates.
Returns true if the line was actually added."
  (let ((line (string-trim '(#\Space #\Tab) line)))
    (when (and (plusp (length line))
               (or (zerop (history-count))
                   (not (string= line (history-ref (1- (history-count)))))))
      (vector-push-extend line *history*)
      (history-trim)
      (when *history-persist* (ignore-errors (append-history-line line)))
      t)))

(defun append-history-line (line)
  "Append a single LINE to the history file."
  (with-open-file (out (history-file)
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create
                       :external-format :utf-8)
    (write-line line out)))

(defun load-history ()
  "Load history entries from disk into *HISTORY*."
  (setf (fill-pointer *history*) 0)
  (let ((file (history-file)))
    (when (probe-file file)
      (handler-case
          (with-open-file (in file :external-format :utf-8)
            (loop for line = (read-line in nil :eof)
                  until (eq line :eof)
                  when (plusp (length line))
                    do (vector-push-extend line *history*)))
        (error () nil)))
    (history-trim))
  (history-count))

(defun save-history ()
  "Rewrite the entire history file from *HISTORY* (used by `history -c`)."
  (handler-case
      (with-open-file (out (history-file)
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create
                           :external-format :utf-8)
        (loop for e across *history* do (write-line e out)))
    (error () nil)))

(defun history-clear ()
  "Empty both in-memory and on-disk history."
  (setf (fill-pointer *history*) 0)
  (save-history))

(defun history-search-backward (query start)
  "Return the index of the newest entry at or before START whose text
contains QUERY, or NIL.  Searches from index START downward."
  (loop for i from (min start (1- (history-count))) downto 0
        when (search query (history-ref i) :test #'char-equal)
          do (return i)))

;;; --- Structured, queryable history --------------------------------------
;;;
;;; Each executed line is also recorded as a plist -- (:text :status :cwd
;;; :time :commands) -- so history can be queried as Lisp data rather than
;;; grepped as text.  COMMANDS is a list of argv lists (or (:lisp) for a Lisp
;;; stage) across the line's pipelines.

(defun records-file ()
  (merge-pathnames ".sbsh_history.sexp" (user-homedir-pathname)))

(defun entry-text (entry) (getf entry :text))
(defun entry-status (entry) (getf entry :status))
(defun entry-cwd (entry) (getf entry :cwd))
(defun entry-time (entry) (getf entry :time))
(defun entry-commands (entry) (getf entry :commands))

(defun failed-p (entry)
  "True if the recorded line exited with a non-zero status."
  (not (eql 0 (entry-status entry))))

(defun command-used-p (name entry)
  "True if ENTRY's line ran a command named NAME in any of its pipelines."
  (some (lambda (cmd) (and (consp cmd) (stringp (first cmd))
                           (string= (first cmd) name)))
        (entry-commands entry)))

(defun history-where (predicate)
  "Return the list of history entries satisfying PREDICATE (newest last)."
  (loop for e across *history-records*
        when (funcall predicate e) collect e))

(defun append-record (entry)
  (with-open-file (out (records-file)
                       :direction :output :if-exists :append
                       :if-does-not-exist :create :external-format :utf-8)
    (let ((*print-readably* nil) (*print-length* nil) (*print-level* nil))
      (prin1 entry out)
      (terpri out))))

(defun record-history-line (line)
  "Store a structured record for the just-executed LINE."
  (let ((entry (list :text line
                     :status *last-status*
                     :cwd (ignore-errors (sb-posix:getcwd))
                     :time (get-universal-time)
                     :commands (reverse *line-commands*))))
    (vector-push-extend entry *history-records*)
    (when *history-persist* (ignore-errors (append-record entry)))
    entry))

(defun load-records ()
  "Load structured history records from disk."
  (setf (fill-pointer *history-records*) 0)
  (let ((file (records-file)))
    (when (probe-file file)
      (handler-case
          (with-open-file (in file :external-format :utf-8)
            (loop for form = (read in nil :eof)
                  until (eq form :eof)
                  when (consp form) do (vector-push-extend form *history-records*)))
        (error () nil))))
  (fill-pointer *history-records*))
