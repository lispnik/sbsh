;;;; terminal.lisp --- Raw/cooked terminal mode and window size via termios.

(in-package #:sbsh)

;;; --- Window size (TIOCGWINSZ) -------------------------------------------

(sb-alien:define-alien-type nil
  (sb-alien:struct winsize
    (ws-row sb-alien:unsigned-short)
    (ws-col sb-alien:unsigned-short)
    (ws-xpixel sb-alien:unsigned-short)
    (ws-ypixel sb-alien:unsigned-short)))

(sb-alien:define-alien-routine ("ioctl" %ioctl-winsize) sb-alien:int
  (fd sb-alien:int)
  (request sb-alien:unsigned-long)
  (ws (sb-alien:* (sb-alien:struct winsize))))

(defconstant +tiocgwinsz+
  #+darwin #x40087468
  #+linux  #x5413
  #-(or darwin linux) #x5413
  "ioctl request number for fetching the terminal window size.")

(defun terminal-size (&optional (fd *shell-terminal*))
  "Return (values ROWS COLS) for terminal FD, defaulting to 24x80."
  (handler-case
      (sb-alien:with-alien ((ws (sb-alien:struct winsize)))
        (if (zerop (%ioctl-winsize fd +tiocgwinsz+ (sb-alien:addr ws)))
            ;; An unset window size (common on freshly-forked Linux ptys)
            ;; reports 0; fall back to a sane 80x24 rather than 0/1.
            (let ((rows (sb-alien:slot ws 'ws-row))
                  (cols (sb-alien:slot ws 'ws-col)))
              (values (if (plusp rows) rows 24)
                      (if (plusp cols) cols 80)))
            (values 24 80)))
    (error () (values 24 80))))

(defun terminal-columns (&optional (fd *shell-terminal*))
  (nth-value 1 (terminal-size fd)))

;;; --- Raw / cooked mode --------------------------------------------------

(defun enter-raw-mode (&optional (fd *shell-terminal*))
  "Switch terminal FD into raw mode suitable for line editing.
Returns the previous (cooked) attributes so the caller can restore them.
Disables canonical mode, echo, and signal generation so the editor can
interpret every keystroke itself; leaves output post-processing on so a
bare newline still expands to CR/LF."
  (let* ((saved (sb-posix:tcgetattr fd))
         (raw (sb-posix:tcgetattr fd)))
    (setf (sb-posix:termios-lflag raw)
          (logandc2 (sb-posix:termios-lflag raw)
                    (logior sb-posix:icanon sb-posix:echo sb-posix:isig)))
    (setf (sb-posix:termios-iflag raw)
          (logandc2 (sb-posix:termios-iflag raw)
                    (logior sb-posix:ixon sb-posix:icrnl)))
    (setf (aref (sb-posix:termios-cc raw) sb-posix:vmin) 1)
    (setf (aref (sb-posix:termios-cc raw) sb-posix:vtime) 0)
    (sb-posix:tcsetattr fd sb-posix:tcsaflush raw)
    saved))

(defun restore-mode (saved &optional (fd *shell-terminal*))
  "Restore terminal FD to the previously saved attributes SAVED."
  (when saved
    (sb-posix:tcsetattr fd sb-posix:tcsaflush saved)))

(defmacro with-raw-mode ((fd) &body body)
  "Run BODY with terminal FD in raw mode, restoring the prior mode after."
  (let ((saved (gensym "SAVED")) (f (gensym "FD")))
    `(let* ((,f ,fd)
            (,saved (enter-raw-mode ,f)))
       (unwind-protect (progn ,@body)
         (restore-mode ,saved ,f)))))
