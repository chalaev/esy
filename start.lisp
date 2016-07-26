;; common.lisp Time-stamp: <2016-07-19 21:58 EDT by Oleg SHALAEV http://chalaev.com >
;; used by main.lisp

;; Some integer parametrs may have (string) vaulues like "10m" which require parsing:
(handler-case
    (libconfig:with-read-config-file (namestring mainConfFile)
      (defparameter hosts  (libconfig:read-setting "hosts" :default (list hostname)))
      (bt:with-lock-held (*log-queue-lock*); adjusting loglevel
	(setf *logLevel*  (libconfig:read-setting "logLevel" :default maxLogLevel)))
      (defparameter *maxFilesPerDir*  (libconfig:read-setting "maxFilesPerDir" :default 50))
      (defparameter *maxFileSize*  (parseNumberTimesUnit (libconfig:read-setting "maxFileSize" :default "1m")))
      (defparameter *maxDirRecursion*  (libconfig:read-setting "maxDirRecursion" :default 7))
      (defparameter *rootdirs*   (libconfig:read-setting "rootdirs" :default (list homeDir)))
      ;; defaults for *goodFiles* and *junkFiles* must contain hashes in order to be as flexible as generalized patterns in exampe.conf
      (defparameter goodFiles0 ; files matching these patterns will be saved
	(recursive-subst '(("f" :regular-file) ("d" :directory))
			 (libconfig:read-setting "importantFiles" :default '("*.txt" "*.org" "*.tex" "*.lisp"))))
      (defparameter junkFiles0
	(recursive-subst '(("f" :regular-file) ("d" :directory))
			 (libconfig:read-setting "junkFiles" :default  '("tmp*" "*.tmp" "*.lock" "*.log" "*.aux"))))
      (defparameter *doNotWatchDirs*  (libconfig:read-setting "doNotWatchDirs" :default 'nil)))
  (libconfig:conf-file-read-error () (errExit "could not read config file"))
  (libconfig:config-parse-error () (errExit "config file (libconfig-) syntax error")))

(defparameter *goodFiles* (WCorHashToRegEx goodFiles0))
(defparameter *junkFiles* (WCorHashToRegEx junkFiles0))
(pushnew (dirname (namestring *log-file*)) *doNotWatchDirs*); log-file is always changed, so we avoid monitoring its directory

(unless (member hostname
		(mapcar #'(lambda (x) (gethash 'hostname x)) hosts)
		:test 'equal)
  (errExit  "/config file: seems that this .conf file is not designed for this host"))

;; (defvar watchedDirs 'nil); also subdirectories of them are watched
(unless (typep *rootdirs* 'cons) (errExit (format 'nil "in config file: *rootdirs* must be CONS, not ~a!" (type-of *rootdirs*))))
(defun filesEqualS (x y) (equal (f-name x) (f-name y))); not to use with allFiles: what if a file was erased and then a directory with the same name created?
(defun filesEqual (x y) (and (filesEqualS x y) (equal (type-of x) (type-of y))))

(defun contains (parent child)
  (and (cl-ppcre:all-matches  child parent)
       (= 0 (first (cl-ppcre:all-matches child parent)))))

(tlog :info "Preparing to watch ~d directories together with their descendants" (length *rootdirs*))

(flet ((pdt (x y)
	 (let* ((fx (f-name x)) (fy (f-name y))
		(res (or
		     (filesEqual x y)
		     (contains  fx fy)
		     (contains  fy fx))))
	   (when res (tlog :warning "two directories: ~s and ~s belong to the same tree; one will be ignored" fx fy))
	   res)))
  (loop for aRootDir in *rootdirs* do
       (typecase aRootDir
	 (string (if (osicat:directory-exists-P aRootDir)
		     (pushnew (make-instance 'catalog
					     :name aRootDir
					     :maxDirRecursion *maxDirRecursion*
					     :maxFileSize *maxFileSize*
					     :maxFilesPerDir *maxFilesPerDir*
					     :junkFiles *junkFiles*
					     :goodFiles *goodFiles*)
			      *allFiles* :test #'pdt)
		     (tlog :error "(ignored) root directory ~s does not exist" aRootDir)))
	 (hash-table (if (osicat:directory-exists-P (gethash 'name aRootDir))
			 (pushnew (make-instance 'catalog
						 :name (nth-value 0 (gethash 'name aRootDir))
						 :maxDirRecursion (nth-value 0 (gethash 'maxDirRecursion aRootDir *maxDirRecursion*))
						 :maxFileSize (nth-value 0 (gethash 'maxFileSize aRootDir *maxFileSize*))
						 :maxFilesPerDir (nth-value 0 (gethash 'maxFilesPerDir aRootDir *maxFilesPerDir*))
						 :junkFiles (WCorHashToRegEx (nth-value 0 (gethash 'junkFiles aRootDir junkFiles0)))
						 :goodFiles (WCorHashToRegEx (nth-value 0 (gethash 'goodFiles aRootDir goodFiles0))))
				  *allFiles* :test #'pdt)
			 (tlog :error "(ignored) root directory ~s does not exist" (gethash 'name aRootDir))))
	 (otherwise (tlog :warning
			  "during config parsing: in *rootdirs* array, ignored an element ~a of (wrong) ~a type"
			  aRootDir (type-of aRootDir))))))

(tlog :debug "There are ~d objects in *allFiles*" (length *allFiles*))
;; 
;; recover if previous run ended unexpectedly (does not work):
;; (when (file-exists-p backupFile); в случае, если предыдущий раз программа аварийно завершила работу
;; (tlog :info "Recovering configuration and modification database from ~s~%" (namestring backupFile))
;;   (recoverDatabase backupFile))
;; (when (or (osicat:directory-exists-p localScript) (protected-regular-file-exists-p localScript)) (protected-delete-file localScript))
;; (when (or (osicat:directory-exists-p remoteScript) (protected-regular-file-exists-p remoteScript)) (protected-delete-file remoteScript))

;; (defconstant lnm (apply 'min (mapcar #'(lambda (mainRootDir)
;;    (- (length (namestring mainRootDir)) 1)) *rootdirs*))); # of symbols to be cut from pathnames in sys.messages
