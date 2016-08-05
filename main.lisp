;; Time-stamp: <2016-08-04 21:34 EDT by Oleg SHALAEV http://chalaev.com >
;; Next (major) release candidate with significant changes.
;; M-x slime-pwd     Print the current directory of the Lisp process. 
;; M-x slime-cd     Set the current directory of the Lisp process.
;; ~/PROGRAMMISMO/esy/lisp/
(ql:quickload '(:sb-posix :cl-ppcre :osicat :libconfig :inotify :cl-log :local-time :bordeaux-threads))
(defvar movedFrom 'nil) (defvar newDirsToWatch 'nil) (defvar movedFrom 'nil) (defvar thisMovedFrom 'nil)

(defconstant hostname (sb-unix:unix-gethostname))
(defconstant homeDir (namestring (user-homedir-pathname)))
(defconstant mainConfDir (merge-pathnames #p".esy/" (user-homedir-pathname)))
(ensure-directories-exist mainConfDir)
(defconstant mainConfFile (merge-pathnames (concatenate 'string hostname ".conf") mainConfDir))
(defparameter *log-file* (merge-pathnames "log.txt" mainConfDir)); log file name, required before we load log.lisp
(defconstant localScript (merge-pathnames "local.sh" mainConfDir))
(defconstant remoteScript (merge-pathnames (concatenate 'string "from-" hostname ".sh") mainConfDir))
(defparameter *logLevel* 0); 0 is the most informative log level
;; (mapcar #'(lambda (x) (load x :verbose t)) '("log.lisp" "system.lisp" "common.lisp"))
(mapcar #'(lambda (x) (load x :verbose t)) '("log.lisp" "system.lisp" "common.lisp"  "start.lisp" "inotify.lisp"))
(tlog :info "log started")
(format t "~a Events/warnings/errors are logged into the file ~s~%" (strTime) (namestring *log-file*))

;; следующая команда не пройдёт в случае, если имеются ошибочные ссылки (удаляются командой symlinks -rdv ~)
;; или неверно (с точки зрения используемой мною utf8 кодировки) закодированные имена файлов:
(defvar allSubDirs 'nil)
(mapcar #'(lambda (rootDir); rootDir is a catalog object
	    (osicat:walk-directory ; unfortunately it walks over subdirs RANDOMLY
	     rootDir
	     #'(lambda (x) (let ((fn (namestring (osicat:absolute-pathname x))))
			(when (and (isDir fn) (not (string= fn rootDir))) (push fn allSubDirs))))
	     :directories t ; this option ALLOWS recursive sub-directories scanning, otherwise will walk only through files
	     :if-does-not-exist :ignore))
	(mapcar 'f-name *allFiles*))
(when allSubDirs ; this block is repeated below once again
  (mapcar #'(lambda (x) (watch 'catalog x :doNotChmod t)) (sort allSubDirs #'string-lessp))
  (setf allSubDirs 'nil))

(defconstant moveDelay 100); (miliseconds) -- time we wait between two events: "moved from" and "moved to"
(defun inotify-worker () ; fast-reacting on inotify events, to be run in a separate thread
  (handler-case ; exception arises, e.g., for badly named directories, such as ~/www/server/etc/sy\*/
      (inotify:with-inotify (myWatch (mapcar #'(lambda (x) (list (remove #\\ x) monitoredEvents))
					     (mapcar 'f-name (selectDirs *allFiles*))))
	(tlog :info "started monitoring ~d directories." (length (selectDirs *allFiles*)))
	(loop ; this loop endlessly reads events
	   (progn
	     ;; (tlog :debug "restarting main loop")
	     (act-on-inotify (inotify:read-events myWatch)); later it will just notify separate thread
	     (when movedFrom ; in case we received "moved from", and then for a long time did not receive "moved to",
	       (let ((ct (get-internal-real-time))) ; we assume that this file was moved to a non-monitored directory.
		 (mapcar #'(lambda (x) (rm (watch 'file (first x) :doNotChmod t)))
			 (remove-if-not #'(lambda (x) (> ct (+ moveDelay (third x)))) movedFrom))
		 (setf movedFrom (remove-if #'(lambda (x) (> ct (+ moveDelay (third x)))) movedFrom))))
	     (when newDirsToWatch ; later this should also be treated in a separate thread
	       (mapcar #'(lambda (newDir) (progn
		       (tlog :debug "start monitoring the newly created directory ~s" newDir)
		       (handler-case (inotify:add-watch myWatch (pathname newDir) monitoredEvents)
			 (iolib.syscalls:enoent () (tlog :error "(ignored) failed to start watching inexistent directory ~s~%" newDir)))
		       (osicat:walk-directory
			newDir
			#'(lambda (x) (let* ((pfn (osicat:absolute-pathname x))
					(fn (namestring pfn)))
				   (when (and (not (string= fn newDir)) (isDir fn) (not (junkP fn)))
				     (push fn allSubDirs)
				     (handler-case
					 (inotify:add-watch myWatch pfn monitoredEvents)
				       (iolib.syscalls:enoent () (tlog :error "(ignored) failed to start watching inexistent directory ~s~%" fn))))))
			:directories t :if-does-not-exist :ignore ; :test avoids deschending into junk sub-directories:
			:test #'(lambda (y) (let ((fn (namestring (osicat:absolute-pathname y))))
					 (or (isFile fn) (and (isDir fn) (not (junkP fn)))))))))
		      newDirsToWatch)
	       (setf newDirsToWatch 'nil))
	     (when allSubDirs
	       (mapcar #'(lambda (x) (watch 'catalog x)) (sort allSubDirs #'string-lessp))
	       (setf allSubDirs 'nil)))))
    (iolib.syscalls:enoent ()
    (tlog :error "one of the directories was removed during the main loop start"))))

(defvar *inotifyThread* (bt:make-thread #'inotify-worker :name "inotify-worker")); launch inotify thread
(tlog :debug "~d threads are active now" (length (bt:all-threads)))

;; only *ctl-do-read* is watched with inotify:
(unless (boundp '*ctl-do-read*) (defparameter *ctl-do-read* (namestring (merge-pathnames "do-read.ctl" mainConfDir)))); a directory
;; when *ctl-do-read* is created, *ctl-message* must be read:
(unless (boundp '*ctl-message*) (defparameter *ctl-message* (namestring (merge-pathnames "message.ctl" mainConfDir))))
		 
(defun split-by-one-space (string); http://cl-cookbook.sourceforge.net/strings.html
    "Returns a list of substrings of string divided by ONE space each.
Note: Two consecutive spaces will be seen as if there were an empty string between them."
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
       while j))
(defun make-keyword (name) (values (intern (string-upcase (car (split-by-one-space name))) "KEYWORD")))

(inotify:with-inotify (mwat (list (list (namestring mainConfDir) inotify:in-create)))
  (block ctl
    (loop
       (dolist (te (inotify:read-events mwat)); wait for events
	 (when (string= *ctl-do-read* 
			(concatenate 'string
				     (namestring (inotify:watch-pathname (inotify:event-watch te)))
				     (inotify:event-name te)))
	   (handler-case
	       (with-open-file (message *ctl-message* :direction :input)
		 (let ((ctl-command (read-line message))); only the first line of the message file will be read
		   (case  (make-keyword ctl-command)
		     (#.:exit  (sync) (return-from ctl))
		     (#.:quit (return-from ctl)); quit the main loop
		     (otherwise (tlog :error "(ignored) received unknown ctl command")))))
	     (sb-int:simple-file-error ()
	       (tlog :error "can not read *ctl-message* file ~s" *ctl-message*)))
	   (eraseFileOrDir *ctl-do-read* *ctl-message*))))))

(eraseFileOrDir *ctl-do-read* *ctl-message*)

(tlog :info "log stopped")
(sleep 1) ;(flush-log)
(mapcar 'bt:destroy-thread (list *inotifyThread* *logThread*))
(format t "~d threads remained~%" (length (bt:all-threads)))
(sync)
(exit)
