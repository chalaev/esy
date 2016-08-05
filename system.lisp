;; system.lisp Time-stamp: <2016-07-17 16:21 EDT by Oleg SHALAEV http://chalaev.com >
;; Some system functions
;; todo: remove some of the (unused) functions defined here
;; (require 'sb-posix)

(defun lsFiles (thisDir) "returns the list of files (not directories or links) in the specified directory"
  (remove-if-not #'(lambda (x) (equal (osicat:file-kind x) :regular-file)) (osicat:list-directory thisDir)))

(defun fileGroupID (fileName) (sb-posix:stat-gid (sb-posix:stat fileName)))
(defun isDir  (fileName) (equal :directory    (osicat:file-kind fileName)))
(defun isFile (fileName) (equal :regular-file (osicat:file-kind fileName)))

(defun GroupName (gid) (nth-value 0 (nix:getgrgid gid)))
(defun unprotected-fileGroup (fileName) (GroupName (fileGroupID fileName)))
(defun fileGroup (fullName)
  (handler-case (unprotected-fileGroup fullName)
    (sb-posix:syscall-error ()
      (tlog :error "failed to reveal (inexistent?) file (~s) group" fullName))))
(defun GroupID (name) (sb-posix:group-gid (sb-posix:getgrnam name)))

(defun fileSize (fullName)
  (handler-case (ql-util:file-size fullName)
    (sb-int:simple-file-error ()
      (tlog :error "failed to reveal (inexistent?) file (~s) size" fullName)))); must return zero (or nil?) in case of an error

(defun fixDir (dirPathName) "adding separator at the end of the directory name if necessary"
   (if (equal "/" (subseq dirPathName (- (length dirPathName) 1))) dirPathName (concatenate 'string  dirPathName "/")))

(defun strDate () "gets human-readable date and time string suitable for touch command"
  (multiple-value-bind (second minute hour date month year day-of-week dst-p tz) (get-decoded-time)
    (when dst-p (decf tz)); daylight savings time
    (format 'nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d GMT~@d" year month date hour minute second (- tz))))

(defun strTime () "gets human-readable time string"
       (multiple-value-bind (second minute hour) (get-decoded-time)
	 (format 'nil "~2,'0d:~2,'0d:~2,'0d" hour minute second)))

(defun unprotected-fileDate (fileName) "gets human-readable date of a file compatible with touch command" ; example: "2015-03-10 11:31:27 GMT-4"
       (with-open-file (s fileName)
	 (multiple-value-bind (sec minute hour date month year day-of-week dst-p tz) (decode-universal-time (file-write-date s))
	   (when dst-p (decf tz)); daylight savings time
	   (format 'nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d GMT~@d" year month date hour minute sec (- tz)))))
;; ← the output is suitable for touch command, e.g. touch --date="2015-03-10 11:31:27 GMT-4" file
(defun fileDate (fullName) "gets human-readable date of a file compatible with touch command"
  (handler-case (unprotected-fileDate fullName)
    (SB-INT:SIMPLE-FILE-ERROR ()
      (tlog :error "failed to reveal (inexistent?) file (~s) date" fullName))))

(defun fileMod (fileName) "returns file permissions in the form suitable for the chmod command"
  (let ((tm (osicat:file-permissions  fileName)))
    (apply '+ (mapcar #'(lambda (x)
			  (case x
			    (  :user-read 400)
			    ( :user-write 200)
			    (  :user-exec 100)
			    ( :group-read 040)
			    (:group-write 020)
			    ( :group-exec 010)
			    ( :other-read 004)
			    (:other-write 002)
			    ( :other-exec 001))) tm))))
;; ← to do exception handling      (tlog :error "failed to reveal (inexistent?) file (~s) permissions" fullName))))

(defun file-exists-p (fullName)
  (handler-case (osicat:regular-file-exists-p fullName)
    (sb-int:simple-file-error ()
      (tlog :error "(ignored) file ~s possibly erased during the test" fullName))
    (simple-error () 
      (tlog :error "file-exists-p: some STRANGE error with the file file ~s" fullName))))

(defun eraseFileOrDir (name &rest others)
  "checks if file(s) or directory(ies) exist(s), and then erases it/them"
  (mapcar #'(lambda (fn)
	      (handler-case (progn
		(when (file-exists-p fn) (delete-file fn))
		(when (osicat:directory-exists-p fn) (delete-directory fn :recursive T)))
		(sb-int:simple-file-error ()
		  (tlog :error "failed to delete inexistent file ~s~%" (namestring fn)))))
	  (cons name others)))

(defmacro timing (&body forms) ;http://cl-cookbook.sourceforge.net/dates_and_times.html
    (let ((real1 (gensym))
	    (real2 (gensym))
	    (run1 (gensym))
	    (run2 (gensym))
	    (result (gensym)))
    `(let* ((,real1 (get-internal-real-time))
	      (,run1 (get-internal-run-time))
	      (,result (progn ,@forms))
	      (,run2 (get-internal-run-time))
	    (,real2 (get-internal-real-time)))
       ;; (/ (- ,run2 ,run1) internal-time-units-per-second)
       (values	,result
 (/ (- ,real2 ,real1) internal-time-units-per-second)))))
