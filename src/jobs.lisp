;;;; jobs.lisp --- Job and process bookkeeping using waitpid.

(in-package #:sbsh)

(defstruct proc
  pid
  (status :running)   ; :running :stopped :done
  (exit-code 0)
  command)            ; display string

(defstruct job
  id
  pgid
  (procs '())
  command
  (background nil)
  (notified nil)
  (tmodes nil))     ; saved terminal modes when the job was stopped

(defvar *jobs* '() "Active jobs, most-recent first.")
(defvar *job-counter* 0 "Monotonic counter for job ids.")

(defun next-job-id ()
  (incf *job-counter*))

(defun add-job (job)
  (push job *jobs*)
  job)

(defun remove-job (job)
  (setf *jobs* (remove job *jobs*)))

(defun find-job (id)
  (find id *jobs* :key #'job-id))

(defun current-job ()
  "The most recently referenced job (for bare `fg`/`bg`)."
  (first *jobs*))

(defun job-completed-p (job)
  (every (lambda (p) (eq (proc-status p) :done)) (job-procs job)))

(defun job-stopped-p (job)
  (and (not (job-completed-p job))
       (every (lambda (p) (member (proc-status p) '(:stopped :done)))
              (job-procs job))))

(defun job-exit-code (job)
  "Exit status of the pipeline = exit code of its last process."
  (let ((last (car (last (job-procs job)))))
    (if last (proc-exit-code last) 0)))

(defun find-proc (pid)
  (dolist (job *jobs*)
    (let ((p (find pid (job-procs job) :key #'proc-pid)))
      (when p (return-from find-proc (values p job)))))
  (values nil nil))

(defun mark-process-status (pid status)
  "Update the PROC for PID according to a waitpid STATUS word."
  (multiple-value-bind (proc job) (find-proc pid)
    (declare (ignore job))
    (when proc
      (cond
        ((sb-posix:wifstopped status)
         (setf (proc-status proc) :stopped))
        ((sb-posix:wifsignaled status)
         (setf (proc-status proc) :done
               (proc-exit-code proc) (+ 128 (sb-posix:wtermsig status))))
        ((sb-posix:wifexited status)
         (setf (proc-status proc) :done
               (proc-exit-code proc) (sb-posix:wexitstatus status))))
      t)))

(defun reap-children ()
  "Poll for any children that changed state (non-blocking), updating status.
Used before each prompt to detect finished/stopped background jobs."
  (let ((flags (logior sb-posix:wuntraced sb-posix:wnohang)))
    (handler-case
        (loop
          (multiple-value-bind (pid status) (sb-posix:waitpid -1 flags)
            (if (or (null pid) (<= pid 0))
                (return)
                (mark-process-status pid status))))
      (sb-posix:syscall-error ()
        ;; ECHILD (no children left) is expected; ignore.
        nil))))

(defun wait-for-job (job)
  "Block until JOB is either stopped or completed."
  (loop until (or (job-stopped-p job) (job-completed-p job))
        do (handler-case
               (multiple-value-bind (pid status)
                   (sb-posix:waitpid (- (job-pgid job)) sb-posix:wuntraced)
                 (when (and pid (> pid 0))
                   (mark-process-status pid status)))
             (sb-posix:syscall-error ()
               ;; No such children left; treat the job as done.
               (dolist (p (job-procs job))
                 (when (eq (proc-status p) :running)
                   (setf (proc-status p) :done)))
               (return)))))

(defun mark-job-running (job)
  "Reset every stopped process in JOB back to running (after SIGCONT)."
  (dolist (p (job-procs job))
    (when (eq (proc-status p) :stopped)
      (setf (proc-status p) :running)))
  (setf (job-notified job) nil))

(defun job-status-string (job)
  (cond ((job-completed-p job) "Done")
        ((job-stopped-p job) "Stopped")
        (t "Running")))

(defun format-job (job &optional (stream *standard-output*) current)
  (format stream "[~D]~A  ~A~vT~A~%"
          (job-id job)
          (cond ((eq job current) "+") (t " "))
          (job-status-string job)
          14
          (job-command job)))

(defun notify-finished-jobs ()
  "Report background jobs that finished or stopped, and purge completed ones."
  (dolist (job (reverse *jobs*))
    (when (and (job-background job)
               (not (job-notified job))
               (or (job-completed-p job) (job-stopped-p job)))
      (format t "[~D]~A  ~A~vT~A~%"
              (job-id job) "+" (job-status-string job) 14 (job-command job))
      (setf (job-notified job) t)))
  (setf *jobs* (remove-if #'job-completed-p *jobs*)))
