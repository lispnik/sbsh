;;;; conditions.lisp --- Condition types for interactive error recovery.

(in-package #:sbsh)

(define-condition shell-error (error)
  ((message :initarg :message :initform nil :reader shell-error-message))
  (:report (lambda (c s)
             (format s "~A" (or (shell-error-message c) "shell error")))))

(define-condition command-not-found (shell-error)
  ((name :initarg :name :reader command-not-found-name)
   (suggestions :initarg :suggestions :initform '()
                :reader command-not-found-suggestions))
  (:report (lambda (c s)
             (format s "~A: command not found" (command-not-found-name c)))))

;;; --- Levenshtein distance, for "did you mean?" suggestions --------------

(defun levenshtein (a b &optional (limit most-positive-fixnum))
  "Edit distance between strings A and B, capped for early exit at LIMIT."
  (let* ((la (length a)) (lb (length b))
         (prev (make-array (1+ lb)))
         (cur (make-array (1+ lb))))
    (dotimes (j (1+ lb)) (setf (aref prev j) j))
    (dotimes (i la)
      (setf (aref cur 0) (1+ i))
      (let ((rowmin (aref cur 0)))
        (dotimes (j lb)
          (let ((cost (if (char-equal (char a i) (char b j)) 0 1)))
            (setf (aref cur (1+ j))
                  (min (1+ (aref cur j))
                       (1+ (aref prev (1+ j)))
                       (+ cost (aref prev j)))))
          (setf rowmin (min rowmin (aref cur (1+ j)))))
        (when (> rowmin limit)
          (return-from levenshtein (1+ limit))))
      (rotatef prev cur))
    (aref prev lb)))
