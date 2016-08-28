;;  -*-coding: utf-8;-*-
;; common.lisp Time-stamp: <2016-08-21 12:08 EDT by Oleg SHALAEV http://chalaev.com >
;; used by main.lisp

(defun flatten (lst &optional backtrack acc); ← https://gist.github.com/westerp/cbc1b0434cc2b52e9b10
  (cond ((consp lst) (flatten (car lst) (cons (cdr lst) backtrack) acc))
	(lst (flatten (car backtrack) (cdr backtrack) (cons lst acc)))
	(backtrack (flatten (car backtrack) (cdr backtrack) acc))
	(t (nreverse acc))))

(defun escapeString (str &key (escapeSymbols '(#\Space  #\, #\* #\: #\] #\[)))
  "escapes certain symbols in a given string"
  (concatenate 'string
	       (flatten (loop for x across str collect
			     (if (member x escapeSymbols)
				 (list #\\ x)
				 x))))); ← test: (escapeString "a*bc ,d")

(defun dTildas (fmessage) "double tildas in the string"
  (nth-value 0 (cl-ppcre:regex-replace-all "~" fmessage "~~" :preserve-case t)))

;; Note: junkFiles and ImportantFiles can be smth like
;; ("*.txt","*.org","*.tex","*.lisp","*.c","*.sed", {group="literature"},{name="*.pdf", maxFileSize="20k"}, {pattern="*[^\.]*" type="f"});
;; ← inside one element, logical "and" is applied
;; ← logical "or" is applied between the elements
(defun matchP (fname ifs &key (erased 'nil)) ; ← also to be used in importantP
  (not (loop for IFpat in ifs never ; code here is similar to importantDBE
	    (etypecase IFpat
	      (string (cl-ppcre:scan IFpat fName))
	      (hash-table ; enumerate all possible hash-table entries
	       (let ((namePat (nth-value 0 (gethash 'name IFpat)))
		     (type (nth-value 0 (gethash 'type IFpat))); :directory or :regular-file
		     (maxSize (nth-value 0 (gethash 'maxFileSize IFpat))); size in bytes
		     (group (nth-value 0 (gethash 'group IFpat))))
		 (and
		  (or (not group) (string= group (fileGroup fName)))
		  (or (not type) erased
		      (let ((ft (osicat:file-kind fName)))
			(if (keywordp ft)
			    (equal type (osicat:file-kind fName))
			    (progn
			      (tlog :warning "the file ~s was (re)moved immediately after creation" (dTildas fname))
;; ← otherwise fails with fname="/home/shalaev/PA6OTA/PROGRAMMISMO/learn/clojure/.1.clj~.Ef9uSB"
			      t))))
		  (or (not namePat) (cl-ppcre:scan namePat fName))
		  (or (not (integerp maxSize)) erased
		      (let ((fs (fileSize fName)))
			(if (integerp fs) (<= fs maxSize) t))))))))))

(defun dirname (fn) "extracts parent directory from the full name" ; see also embedded directory-namestring function
       (subseq fn 0 (1+ (car (cl-ppcre:all-matches "/[^/]*.$" fn)))))

(defun basename (fn &key tail) "extracts base name from the full name"
       (let ((m (cl-ppcre:all-matches
	       (concatenate 'string "/[^/]*." (if tail (escapeString tail :escapeSymbols '(#\.)) 'nil) "$")
	       fn)))
	 (if m
	     (apply #'(lambda (x y) (subseq fn (1+ x) (- y (length tail)))) m)
	     (apply #'(lambda (x y) (subseq fn (1+ x) y))
		    (cl-ppcre:all-matches (concatenate 'string "/[^/]*.$") fn)))))
;; see also embedded file-namestring function:
;; (file-namestring "/home/shalaev/tmp.txt") ==> "tmp.txt"

(defun read-property (object field)
  (if (slot-value object field)
      (slot-value object field)
      (read-property (f-parent object) field)))

(defun junkP (fName &key (erased 'nil)) "name-based check if a file or a directory must NOT be saved"
       (let ((pc (DBentry (dirname fName) :type 'catalog))); parent catalog
	 (if pc
	     (matchP fName (read-property pc 'junkFiles) :erased erased)
	     (matchP fName *junkFiles* :erased erased))))

(defun importantP (fName &key (erased 'nil)) "name-based check if a file or a directory must be saved"
       (let ((pc (DBentry (dirname fName) :type 'catalog))); parent catalog
	 (if pc
	     (matchP fName (read-property pc 'goodFiles) :erased erased)
	     (matchP fName *goodFiles* :erased erased))))

;; new (2016-08-09) importantDBE function is intended to check if file/dir status
;; has changed from junk to important or vice versa
;; after it has been included in the DB.
;; (E.g., I could `chgrp important junkFile`)
(defun importantDBE (DBelement) "check if a database element deserves saving"
       (and (not (junkDBE DBelement))
       (let* ((fName (f-name DBelement))
	      (fGroup (f-group DBelement))
	      (pc (DBentry (dirname fName) :type 'catalog))); parent catalog
	 (unless pc (setf pc DBelement)); for rootdirs which have 'nil parent
	 (not (loop for IFpat in ; code here is similar to matchP
		   (read-property pc 'goodFiles)
		 never
		   (etypecase IFpat
		     (string (cl-ppcre:scan IFpat fName))
		     (hash-table ; enumerate all possible hash-table entries
		      (let ((namePat (nth-value 0 (gethash 'name IFpat)))
			    (type (nth-value 0 (gethash 'type IFpat))); :directory or :regular-file
			    (maxSize (nth-value 0 (gethash 'maxFileSize IFpat))); size in bytes
			    (group (nth-value 0 (gethash 'group IFpat))))
			(and
			 (or (not (and group fGroup)) (string= group fGroup))
			 (or (not type) (typep DBelement (ecase type
							   (:regular-file 'file)
							   (:directory 'catalog))))
			 (cl-ppcre:scan namePat fName)
			 (or (not (integerp maxSize)) (<= (fileSize fName) maxSize)))))))))))

(defun junkDBE (DBelement) "check if a database element deserves removal"
       (let* ((fName (f-name DBelement))
	      (fGroup (f-group DBelement))
	      (pc (DBentry (dirname fName) :type 'catalog))); parent catalog
	 (unless pc (setf pc DBelement)); for rootdirs which have 'nil parent
	 (not (loop for IFpat in ; code here is similar to matchP
		   (read-property pc 'junkFiles)
		 never
		   (etypecase IFpat
		     (string (cl-ppcre:scan IFpat fName))
		     (hash-table ; enumerate all possible hash-table entries
		      (let ((namePat (nth-value 0 (gethash 'name IFpat)))
			    (type (nth-value 0 (gethash 'type IFpat))); :directory or :regular-file
			    (maxSize (nth-value 0 (gethash 'maxFileSize IFpat))); size in bytes
			    (group (nth-value 0 (gethash 'group IFpat))))
			(or
			 (and group fGroup) (string= group fGroup))
			 (and type (typep DBelement (ecase type
							   (:regular-file 'file)
							   (:directory 'catalog))))
			 (and namePat (cl-ppcre:scan namePat fName))
			 (and (integerp maxSize) (> (fileSize fName) maxSize)))))))))

;; I guess vectors will not be much faster than lists:
(defvar *allFiles* 'nil); main database; list of monitored files/dirs
(defvar newDirsToWatch 'nil)

(defconstant monitoredEvents (logior inotify:in-attrib inotify:in-create inotify:in-delete inotify:in-delete-self inotify:in-ignored inotify:in-isdir
				     inotify:in-modify inotify:in-moved-from inotify:in-moved-to inotify:in-onlydir inotify:in-dont-follow))

(defconstant mkdir (logior inotify:in-isDIR inotify:in-create))
(defconstant rmdir (logior inotify:in-isDIR inotify:in-delete))
(defconstant mvDirFrom (logior inotify:in-isDIR inotify:in-MOVED-from))
(defconstant mvDirTo (logior inotify:in-isDIR inotify:in-MOVED-to))
(defconstant chmodDir (logior inotify:in-attrib inotify:in-isdir))
(defstruct iEvent "inotify event" (dir 'nil) (file 'nil) (whatsup 'nil))

(defclass file ()
  ((%name :initarg :name :type string :accessor f-name)
   (%parent :type catalog :initarg :parent :accessor f-parent); will be auto-changed when name is changed
   (%prevName :initform 'nil :initarg :prevName :type string :accessor f-prevName); empty unless file was renamed
   (group :initform 'nil :initarg :group :type string :accessor f-group)
   (date :initform 'nil :initarg :date :type string :accessor f-date)
   (perms  :initform 'nil :initarg :perms  :type string :accessor f-perms)
   (%created :initform 'nil :initarg :created :type boolean)
   (%erased :initform 'nil :initarg :erased  :type boolean  :accessor f-erased)
   (%modified :initform 'nil :initarg :modified :type boolean :accessor f-modified)))
;; compare  (mapcar '+ '(1 2) '(3 4)) with multiple-value-call #'+ (values 1 2) (values 3 4))

(defun update-attrs (fileInDB) "update permissions and date for a file or a directory"
       (when fileInDB
	 (setf ; this is one case when file/dir status can be changed from junk to important or vice versa
	  (f-group fileInDB) (fileGroup (f-name fileInDB))
	  (f-date fileInDB)  (fileDate  (f-name fileInDB))
	  (f-perms fileInDB) (fileMod   (f-name fileInDB)))))
;; however, even if the file became unimportant (e.g., due to "chgrp tmp")
;; we should not remove it from the DB
;; because this may be later reversed so it may become important again

;; note that for typep function every catalog is also a file
(defclass catalog (file)
  ((%children :initform 'nil  :type (vector file) :accessor c-children)
   (%maxDirRecursion :initform 7 :initarg :maxDirRecursion :type integer :accessor c-maxDirRecursion); with respect to THIS catalog
   (%maxFileSize :initform 102400 :initarg :maxFileSize :type integer :accessor c-maxFileSize)
   (%maxFilesPerDir :initform 100 :initarg :maxFilesPerDir :type integer :accessor c-maxFilesPerDir)
   (junkFiles :initform 'nil :initarg :junkFiles); a _one_ pattern (every catalog may have its own list of good/bad files)
   (goodFiles :initform 'nil :initarg :goodFiles)))

(defun selectType (fdlist type)
  (ecase type
    ('catalog (remove-if-not #'(lambda (x) (typep x 'catalog)) fdlist))
    ('file   (set-difference fdlist (selectType fdlist 'catalog)))))
(defun selectDirs (fdlist) (selectType fdlist 'catalog))
(defun selectFiles (fdlist) (selectType fdlist 'file))

(defun DBentry (fName &key (type 'nil)) "finds file object with a specified property (default name)"
       (flet ((ft (x y) (string= (f-name y) x)))
	 (if type
	     (find fName (selectType *allFiles* type) :test #'ft)
	     (find fName             *allFiles*       :test #'ft))))

;; ПРОБЛЕМА: каталог продолжает наблюдаться под старым именем; соответственно, события вроде "изменение/создание файла" в новом каталоге
;; будет сопровождаться событием со старым именем каталога!

(defgeneric rm (file)
  (:documentation "Inform the database about file removal"))
(defmethod rm ((object file)); method for both classes
  (setf
   (c-children (f-parent object)) (remove object (c-children (f-parent object)) :count 1)
   (f-erased object) t))
(defmethod rm ((object catalog)); this will be called only for directories
  (mapcar 'rm (c-children object))
  (call-next-method object))
(defmethod rm ((object (eql 'nil)))
  (tlog :debug "I do not care that this file/dir is erased"))

(defun correctName (object)
  (progn
    (setf (f-name object) (concatenate 'string
				       (f-name (f-parent object))
				       (basename (f-name object))))
    (when (typep object 'catalog)
	   (mapcar 'correctName (selectDirs (c-children object))))))

(defgeneric mv (object newName &key movedFromElseWhere type)
  (:documentation "Act on file/dir removal: change the database accordingly (if needed)"))

(defmethod mv ((obj1 (eql 'nil)) (newName string) &key (movedFromElseWhere 'nil) (type 'file))
  (let ((obj2 (watch type newName :doNotChmod t)))
    (tlog :debug "mv1: before the move (mv), the file/dir was not in the DB")
    (when movedFromElseWhere  (setf (f-modified obj2) t) (update-attrs obj2))))

;; Note that even if (junkP newName)=>t, mv is not equivalent to rm
;; because the file or a direcory maight be moved back to a non-junk name:
(defmethod mv ((obj file) (newName string) &key (movedFromElseWhere 'nil) (type 'file)); called also for catalogs
  (let* ((p (f-parent obj)) (nd (dirname newName)) (np (DBentry nd :type 'catalog)))
    (tlog :debug "mv2 ~s ~s" (dTildas (f-name obj)) (dTildas newName))
    ;; (tlog :debug "mv2: chmod= ~s" (f-perms obj))
    (unless (f-prevName obj)
      (tlog :debug "setting previous name to ~s" (dTildas (f-name obj)))
      (setf (f-prevName obj) (f-name obj))); if moved for the first time
    (unless (equal nd (f-name p)); if moved to another directory
      (setf (c-children p) (remove obj (c-children p) :count 1)); first divorse with  ex-parent directory
      (tlog :debug "parent name is ~s and has a DB-name ~s" (dTildas nd) (dTildas (f-name np)))
      (pushnew obj (c-children np)) ; then marry the new one
      (tlog :debug "changing parent name to ~s" (dTildas (f-name np)))
      (setf (f-parent obj) np)
      (correctName obj))
    (setf (f-name obj) newName)
    (when (string= newName (f-prevName obj))
      (tlog :debug "mv2: prevName=name=~s" (dTildas newName))
      (setf (f-prevName obj) 'nil))))

(defun modAsteriscs (str) "transforms * into [^/]*"
  (concatenate 'string
	       (flatten (loop for x across str collect
			     (if (equal x #\*)
				 (list #\[ #\^  #\/ #\] x)
				 x)))))
;; test: (modAsteriscs "*.tex")
(defun shellToRegEx (shellPattern) "transforms shell patterns into regex for the cl-ppcre package"
       (when shellPattern
	 (let ((pat (if (stringp shellPattern) (list shellPattern) shellPattern)))
	 (modAsteriscs
	  (escapeString
	   (subseq
	    (apply 'concatenate
		   (cons 'string
			 (mapcar #'(lambda (x) (concatenate  'string "|/" x "$")) pat)))
	    1) :escapeSymbols '(#\.))))))
;; tests:
;; (shellToRegEx '("*.tex"))
;; (shellToRegEx '(".*.tex")); => "/\\.[^/]*\\.tex$"
;; (cl-ppcre:scan "\\.[^/]*\\.tex$" "/home/shalaev/PA6OTA/PROGRAMMISMO/esy/lisp/#next-commit.tex")
;; (cl-ppcre:scan "\\.[^/]*\\.tex$" "/home/shalaev/PA6OTA/PROGRAMMISMO/esy/lisp/.#next-commit.tex")
;; (cl-ppcre:scan "\\.[^/]*\\.tex$" "/home/shalaev/PA6OTA/PROGRAMMISMO/esy/lisp/.aa/#next-commit.tex")

(defun matchNameOrPattern (fullName mh); mh is a hash containing name or pattern keys
  (let ((pat (if (nth-value 1 (gethash 'pattern mh))
		 (nth-value 0 (gethash 'pattern mh))
		 (shellToRegEx (list (nth-value 0 (gethash 'name mh)))))))
    (when  (cl-ppcre:scan pat fullName)
      (tlog :debug "matchNameOrPattern: matched ~s" (dTildas fullName)) t)))

;; note that it is possible that once upon a time a file existed, then it was erased,
;; and a then directory with the same name appeared
;; Warning: watch should not be applied to root dirs


;; problem → (WATCH 'FILE "/home/shalaev/PA6OTA/DOKYMEHTbI/mail/_fz.GaysXB.theorie13" :ERASED NIL :MODIFIED NIL :DONOTCHMOD NIL)
(defun watch (objType fullName &key (erased 'nil) (modified 'nil) (doNotChmod 'nil))
  "(if it deserves,) add a file or a directory to the *allFiles* database"
  (when fullName ; sometimes it is called with fullName=nil
  (let ((inDB (DBentry fullName :type objType)) (emm (or erased modified doNotChmod)))
    (tlog :debug "watch function received ~s" (dTildas fullName))
    (tlog :debug  "rm=~a, mod=~a, !chmod=~a" erased modified doNotChmod)
    (if inDB
	(progn
	  (tlog :debug "~s is already in the database" (dTildas fullName))
	  (when erased   (setf (f-erased   inDB) t))
	  (when modified (setf (f-modified inDB) t))
	  (when (string= fullName (f-prevName inDB))
	    (tlog :debug "watch: prevName=name=~s" (dTildas fullName))
	    (setf (f-prevName inDB) 'nil)))
	(when ; if this is a newbie: check if it deserves our royal attention
	    (ecase objType
	      (file (and (importantP fullName :erased erased) (not (junkP fullName :erased erased))))
	      (catalog (not (junkP fullName))))
	  (tlog :debug "~s is not in the database" (dTildas fullName))
	  (let* ((parent (DBentry (dirname fullName) :type 'catalog))
		 (newDBelement
		  (handler-case (make-instance objType
					      :name fullName
					      :modified modified
					      :parent parent
					      :group (unless emm (fileGroup fullName))
					      :date  (unless emm (fileDate  fullName))
					      :perms (unless emm  (fileMod fullName)))
		    (osicat-posix:enoent ()
		      (tlog :debug "file ~s was erased during its addition to DB" (dTildas fullName))
		      'nil))))
	    (when newDBelement
	    (when (equal objType 'catalog)
	      ;; (tlog :debug "checking special properties for the directory ~s" (dTildas fullName))
 	      (dolist (hts specialDirectoryOptions) ; they are all hashes
		(when (matchNameOrPattern fullName hts)
		  ;; (tlog :debug "setting special properties for the directory ~s" (dTildas fullName))
		  (setf (c-maxDirRecursion newDBelement) (nth-value 0 (gethash 'maxDirRecursion hts)))
		  (setf (slot-value newDBelement 'goodFiles) (WCorHashToRegEx (nth-value 0 (gethash 'importantFiles  hts))))
		  (setf (slot-value newDBelement 'junkFiles) (WCorHashToRegEx (nth-value 0 (gethash 'junkFiles       hts))))
		  (setf (c-maxFilesPerDir newDBelement) (nth-value 0 (gethash 'maxFilesPerDir hts))))))
	    (pushnew newDBelement *allFiles* :test 'filesEqual)
	    (pushnew newDBelement (c-children parent) :test 'filesEqual)
	    (tlog :info "~s added to DB" (dTildas fullName)); "file changed" in log means that we added it to the DB
	    (find newDBelement *allFiles* :test 'filesEqual))))))))
;; different situations when watch is called:
;; 1. during initialization: recursively adding watched directories
;; 2. when a new file or a directory is created
;; 3. when a file or a directory is removed
;; 4. when chmod or touch
;; (unclear) when rename or move

(defun stripAbsDir (x) (subseq x (length rootDir) (length x)))

;; (defun stripAbsDir (x)
;;   (if (<  (length rootDir) (length x))
;;       (subseq x (length rootDir) (length x))
;;       (progn
;; 	(tlog :debug "strange file ~s" x)
;; 	x)))

(defun sync ()
  (with-open-file (save localScript :direction :output :if-exists :supersede); ~/.esy/local.sh
    ;; (tlog :debug "writing shell scripts in ~~/.esy")
    (format save "#!/bin/sh -e~%# generated ~a by esy on ~s~%echo \"`date '+%Y-%m-%d\ %H:%M:%S'` @ `hostname` = `hostname -I`\" > ~~/.esy/host-id.dat~%timeout 5 emacsclient -e \"(save-some-buffers t)\"~%fetchmail --quit~%cd ~s~%tar jcfv ~~/.esy/from-$(hostname).tbz ~~/.esy/host-id.dat ~s" (strDate) hostname rootDir (namestring remoteScript))
    (mapcar #'(lambda (x) (format save " ~s" (stripAbsDir (f-name x)))) ; these files will be saved into the tar archive
	    (remove-if-not #'(lambda (y) (and (importantDBE y) (f-modified y) (not (f-erased y)))) (selectFiles *allFiles*)))
    (format save "~%rm ~~/.esy/host-id.dat~%# end~%")
    (format save "cd -~%"))
  (with-open-file (script remoteScript :direction :output :if-exists :supersede)
    (format script "#!/bin/sh -e~%# generated ~a by esy on ~s~%# To be excecuted on remote host~%" (strDate) hostname)
    (format script "~%case  `hostname`  in")
    (dolist (allowedHost hosts)
      (unless (string= hostname (nth-value 0 (gethash 'hostname allowedHost)))
	(format script "~%~s)~%rootDir=~s~%;;" (nth-value 0 (gethash 'hostname allowedHost))  (nth-value 0 (gethash 'root allowedHost)))))
    (format script "~%*)~%echo \"this host was not allowed in ~a:.esy/~a.conf, I refuse to change files here, stopping\"~%exit~%;;" hostname hostname)
    (format script "~%esac~%")
    (format script "# moving directories:~%")
    (mapcar #'(lambda (x) (format script "mv -i ~s ~s~%" (stripAbsDir (f-prevName x)) (stripAbsDir (f-name x))))
	    (remove-if-not #'(lambda (x) (and (not (junkDBE x)) (f-prevName x))) (selectDirs *allFiles*)))
    (format script "# moving files:~%")
    (mapcar #'(lambda (x) (format script "mv -i ~s ~s~%" (stripAbsDir (f-prevName x)) (stripAbsDir (f-name x))))
	    (remove-if-not #'(lambda (x) (and (importantDBE x) (f-prevName x))) (selectFiles *allFiles*)))
    (format script "# deleting files and directories:~%for i in ~~/.esy/local.sh")
    (mapcar #'(lambda (x) (format script " ~s" (stripAbsDir (f-name x))))
	    (remove-if-not #'(lambda (x) (or
				     (and (f-erased x)
					  (etypecase x
						     (catalog (not (junkDBE x)))
						     (file    (importantDBE x))))
				     (and (f-perms  x) (not
					  (etypecase x
						     (catalog (not (junkDBE x)))
						     (file    (importantDBE x)))))))
			   *allFiles*))
    (mapcar #'(lambda (x) (format script " ~s" (stripAbsDir (f-prevName x))))
	    (remove-if-not #'(lambda (x) (and (f-prevName x) (not
					  (etypecase x
						     (catalog (not (junkDBE x)))
						     (file    (importantDBE x))))))
	    *allFiles*)) ; moved to unimportant file means "erased"
    (format script "; do if [ -e \"$i\" ]; then rm -r \"$i\" ; fi ; done ~%")
    ;; it is important that all destructive operations (file removal) are done BEFORE new files are extracted:
    (format script "# extracting saved files:~%tar xjfv ~~/.esy/~a.tbz -C $rootDir~%cd \"$rootDir\"~%" (basename (namestring remoteScript) :tail ".sh"))
    (format script "# adjusting ownership (UIDs and GIDs might be different on different hosts):~%")
    (mapcar #'(lambda (x) (format script "chgrp ~a ~s~%" (f-group x) (stripAbsDir (f-name x))))
	    (remove-if-not #'(lambda (x) (and (not (f-erased x)) (f-group x))) *allFiles*))
    (format script "# adjusting dates:~%")
    (mapcar #'(lambda (x) (format script "touch -d ~s ~s~%" (f-date x) (stripAbsDir (f-name x))))
	    (remove-if-not #'(lambda (x) (and (not (f-erased x)) (f-date x))) *allFiles*))
    (format script "# adjusting permissions:~%")
    (mapcar #'(lambda (x) (format script "chmod ~a ~s~%" (f-perms x) (stripAbsDir (f-name x))))
	    (remove-if-not #'(lambda (x) (and (not (f-erased x)) (f-perms x))) *allFiles*))
    (format script "cd -~%")))

(defun copy-hash-table (hash-table); http://stackoverflow.com/questions/26045442/copy-hash-table-in-lisp
  (let ((ht (make-hash-table 
             :test (hash-table-test hash-table)
             :rehash-size (hash-table-rehash-size hash-table)
             :rehash-threshold (hash-table-rehash-threshold hash-table)
             :size (hash-table-size hash-table))))
    (loop for key being each hash-key of hash-table
       using (hash-value value)
       do (setf (gethash key ht) value)
       finally (return ht))))

(defun WCorHashToRegEx (mixedList); mixedList is a mixture of strings and hashes
  "translates wildcards and name-hash-entries into patterns"
  (let ((wildcards (remove-if-not #'(lambda (x) (typep x 'string    )) mixedList))
	(hashes (mapcar 'copy-hash-table  (remove-if-not #'(lambda (x) (typep x 'hash-table)) mixedList))))
    (delete 'nil (cons
		  (shellToRegEx wildcards)
(loop for hts in hashes collect
     (if (nth-value 1 (gethash 'pattern hts))
	 (progn ; if "pattern" field exists, "name" is ignored
	   (setf (gethash 'name hts) (nth-value 0 (gethash 'pattern hts)))
	   (remhash 'pattern hts) hts)
	 (progn
	   (maphash #'(lambda (key val); otherwise we transform wildcard to pattern
			(when (equal key 'name)
			  (setf (gethash key hts) (shellToRegEx val))))
		    hts)
	   hts))))))); I would like to rewrite this ugly function...

(defun parseNumberTimesUnit (str &key
	(modifiers '(("min" 60) ("hours?" 3600) ("days?" 86400) ("weeks?" 604800) ("months?" 18144000) ("y" 6622560000)  ("years?" 6622560000) ("k" 1024) ("m" 1048576)))
        (whenerror 0)); default result when string can not be parsed
  "Parses integer parameters with modifiers k(kbyte=1024), m(mbyte=1024x1024), etc."
  (let ((mind 'nil) (fac 1))
    (loop named mainLoop for p in modifiers do
     (setf mind (car (cl-ppcre:all-matches (concatenate 'string (car p) " *$") str)))
     (when mind
       (setf fac (second p))
       (return-from mainLoop)))
    (unless mind (setf mind (length str)))
    (handler-case 
	(* fac (nth-value 0 (parse-integer (subseq str 0 mind))))
      (SB-INT:SIMPLE-PARSE-ERROR ()
	(progn
	  (tlog :error "string ~s can not be parsed, will use the default value ~d for the result" str whenerror)
	  whenerror)))))
;; test: (mapcar 'parseNumberTimesUnit '(" 100 " "10 d" "1 day" "2 days" "1k" "2m"))

(defun mapAll (f y) "MapAll Mathematica (R) function"
  (typecase y
    (cons (mapcar #'(lambda (x) (mapAll f x)) y))
    (hash-table (progn (maphash #'(lambda (key val) (setf (gethash key y) (mapAll f val))) y) y))
    (otherwise (funcall f y)))); semi-tested 2016-07-08

;; I guess I should use reduce instead of Fold:
(defun Fold (f x yList) "A Mathematica (R) Fold function"
       (let ((result x))
	 (loop for y in yList do
	      (setf result (funcall f result y)))
	 result)); semi-tested 2016-07-08

(defun recursive-subst (substRules object) "transforms object according to substRules"
       (Fold ; I guess I should use reduce instead of Fold here
	#'(lambda (structure rule)
	    (mapAll #'(lambda (obj) (if (equal (car rule) obj) (second rule) obj)) structure))
	object substRules)); tested 2016-07-08

(defun recursive-parse-units (structure) "recursively applies parseNumberTimesUnit"
	    (mapAll #'(lambda (obj) (if (and (stringp obj) (cl-ppcre:scan "^ *\\d+ *[a-zA-Z]+$" obj))
				   (parseNumberTimesUnit obj)
				   obj)) structure))
			   
(defun mps (str1 str2); arguments might be path names…
  (merge-pathnames (escapeString (namestring str1)) (escapeString (namestring str2))))

(defun errExit (message)
  (tlog :error message) ;(flush-log)
  (sleep 1)
  (exit))
