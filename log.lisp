;; Time-stamp: <2016-06-26 09:19 EDT by Oleg SHALAEV http://chalaev.com > 
;; defining separate thread for logging; see tests/log.lisp
;; to do: (defun flush-log () ...)
;; ← this function must ensure that log thread is started and ready to receive/output messages

;; (defparameter *logLevel*)
(defconstant minLogLevel 0) (defconstant maxLogLevel 4)
(unless (boundp '*logLevel*) (defvar *logLevel* maxLogLevel))
(when (< *logLevel* minLogLevel) (setf *logLevel* minLogLevel))
(when (> *logLevel* maxLogLevel) (setf *logLevel* maxLogLevel))

;; https://www.common-lisp.net/project/local-time/manual.html
(defparameter logTimeFormat
  '((:YEAR 4) #\-  (:MONTH 2) #\- (:DAY 2) #\  (:HOUR 2) #\: (:MIN 2)  #\: (:SEC 2) #\. :msec ))
;; test: (local-time:format-timestring 'nil (local-time:now) :format logTimeFormat)

(defmethod cl-log:format-message ((self cl-log:formatted-message))
  (format nil "~a ~a ~?~&"
	  (let ((ts (cl-log:message-timestamp self)))
	    (local-time:format-timestring
	     'nil
	     (local-time:universal-to-timestamp
	      (cl-log:timestamp-universal-time ts)
	      :nsec (* 1000000 (cl-log:timestamp-fraction ts)))
	     :format logTimeFormat))
	  (cl-log:message-category self)
	  (cl-log:message-description self)
	  (cl-log:message-arguments self)))


(defvar *log-queue-lock* (bt:make-lock))
(defvar *log-queue-cond* (bt:make-condition-variable))
(defvar *log-queue-cond-lock* (bt:make-lock))
(defvar *log-queue* nil)
(defvar *log-queue-time* (get-universal-time))

;; for log flushing:

(defun log-worker ()
  (loop (progn ; endless loop
  (bt:with-lock-held (*log-queue-lock*)
    (mapcar #'(lambda (i) (when (cdr i) (cl-log:log-message (car i) (cdr i)))) (reverse *log-queue*)); fifo
    (setf *log-queue* nil))
  (bt:with-lock-held (*log-queue-cond-lock*); waiting for a message from high-level log-* functions defined below
    (bt:condition-wait *log-queue-cond* *log-queue-cond-lock*)))))

;; the least informative *logLevel* is 4 (no log messages), and the most informative one is 0 (everything is logged).
;; debug messages are the least important, and errors are the most important ones.
(defun tlog (type fmessage &rest vars)
  (let ((level (case type
		 (#.:debug 0)
		 (#.:info 1)
		 (#.:warning 2)
		 (#.:error 3) ; level=3 for errors
		 (otherwise 3))))
    (when (>= level *logLevel*)
      (bt:with-lock-held (*log-queue-lock*)
	(push (cons type
		    (apply #'format 'nil fmessage vars))
	      *log-queue*)
	(if (> (- (get-universal-time) *log-queue-time*) 0)
	    (bt:condition-notify *log-queue-cond*))))))
;; test: (tlog :warning "aaa ~d ggg" *logLevel*)

(setf (cl-log:log-manager) (make-instance 'cl-log:log-manager :message-class 'cl-log:formatted-message))
(unless (boundp '*log-file*) (defparameter *log-file* "/tmp/undefined.log"))
(cl-log:start-messenger 'cl-log:text-file-messenger :filename *log-file*)

(defvar *logThread* (bt:make-thread #'log-worker :name "log-worker")); launch log thread


;; (defun flush-log ()
;;   "ensure that log thread started or flush it before quit"
;;   (bt:with-lock-held (*log-reset-lock*); waiting for a message from high-level log-* functions defined below
;;     (tlog :debug "waiting for the log-flush")
;;     (bt:condition-wait *log-reset-cond* *log-reset-lock*)
;;     (tlog :debug "log flushed")))

;; (defun flush-log ()
;;   (sleep 1)); temporary delay, to be removed 

;; (flush-log); wait (about 1 second) until the thread starts
;; (tlog :info "log starts")
