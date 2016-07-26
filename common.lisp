;; common.lisp Time-stamp: <2016-07-25 17:41 EDT by Oleg SHALAEV http://chalaev.com >
;; used by main.lisp
;; C-h m for keybindings

(defun flatten (lst &optional backtrack acc); copied from ← general.lisp ← https://gist.github.com/westerp/cbc1b0434cc2b52e9b10
  (cond ((consp lst) (flatten (car lst) (cons (cdr lst) backtrack) acc))
	(lst (flatten (car backtrack) (cdr backtrack) (cons lst acc)))
	(backtrack (flatten (car backtrack) (cdr backtrack) acc))
	(t (nreverse acc)))); ← copied from general.lisp

(defun escapeString (str &key (escapeSymbols '(#\Space  #\, #\* #\: #\] #\[))); ← copied from general.lisp
  "escapes certain symbols in a given string"
  (concatenate 'string
	       (flatten (loop for x across str collect
			     (if (member x escapeSymbols)
				 (list #\\ x)
				 x))))); ← test: (escapeString "a*bc ,d")

;; Note: junkFiles and ImportantFiles can be smth like
;; ("*.txt","*.org","*.tex","*.lisp","*.c","*.sed", {group="literature"},{name="*.pdf", maxFileSize="20k"}, {pattern="*[^\.]*" type="f"});
;; ← inside one element, logical "and" is applied
;; ← logical "or" is applied between the elements
(defun matchP (fname ifs) ; ← also to be used in importantP
  (not (loop for IFpat in ifs never
	    (etypecase IFpat
	      (string (cl-ppcre:scan IFpat fName))
	      (hash-table ; enumerate all possible hash-table entries
	       (let ((namePat (gethash 'name IFpat))
		     (type (gethash 'type IFpat)); :directory or :regular-file
		     (maxSize (gethash 'maxFileSize IFpat)); size in bytes
		     (group (gethash 'group IFpat)))
		 (and
		  (or (not group) (equal group (fileGroup fName)))
		  (or (not type) (equal type (osicat:file-kind fName)))
		  (cl-ppcre:scan namePat fName)
		  (or (< maxSize 0) (< (fileSize fName) maxSize)))))))))

(defun dirname (fn) "extracts parent directory from the full name" ; see also embedded directory-namestring function
  (subseq fn 0 (1+ (first (cl-ppcre:all-matches "/[^/]*.$" fn)))))
(defun basename (fn) "extracts base name from the full name" ; see also embedded file-namestring function
       (subseq fn (1+ (first (cl-ppcre:all-matches "/[^/]*.$" fn))) (length fn)))

(defun read-property (object field)
  (if (slot-value object field)
      (slot-value object field)
      (read-property (f-parent object) field)))

(defun junkP (fName) "name-based check if a file or a directory must NOT be saved"
       (let ((pc (DBentry (dirname fName) :type 'catalog)))
	 (if pc
	     (matchP fName (read-property pc 'junkFiles))
	     (matchP fName *junkFiles*))))

(defun importantP (fName) "name-based check if a file or a directory must be saved"
       (let ((pc (DBentry (dirname fName) :type 'catalog)))
	 (if pc
	     (matchP fName (read-property pc 'goodFiles))
	     (matchP fName *goodFiles*))))

(defvar *allFiles* 'nil); main database; список файловых объектов, за которыми происходит слежка
(defvar newDirsToWatch 'nil)

(defconstant monitoredEvents (logior inotify:in-attrib inotify:in-create inotify:in-delete inotify:in-delete-self inotify:in-ignored inotify:in-isdir
inotify:in-modify inotify:in-moved-from inotify:in-moved-to inotify:in-onlydir inotify:in-dont-follow))
(defconstant mkdir (logior inotify:in-isDIR inotify:in-create))
(defconstant rmdir (logior inotify:in-isDIR inotify:in-delete))
(defconstant mvDirFrom (logior inotify:in-isDIR inotify:in-MOVED-from))
(defconstant mvDirTo (logior inotify:in-isDIR inotify:in-MOVED-to))

(defstruct iEvent "inotify event" (dir 'nil) (file 'nil) (whatsup 'nil))

;;(file-namestring "/home/shalaev/tmp.txt") ==> "tmp.txt"
;; (enough-namestring "/home/shalaev/tmp/tmp.txt") ==> "tmp/tmp.txt"
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
	 (setf
	  (f-group fileInDB) (fileGroup (f-name fileInDB))
	  (f-date fileInDB)  (fileDate  (f-name fileInDB))
	  (f-perms fileInDB) (fileMod   (f-name fileInDB)))))

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
    (when movedFromElseWhere  (setf (f-modified obj2) t)  (update-attrs obj2))))

(defmethod mv ((obj file) (newName string) &key (movedFromElseWhere 'nil) (type 'file)); called also for catalogs
  (let* ((p (f-parent obj)) (nd (dirname newName)) (np (DBentry nd :type 'catalog)))
    (tlog :debug "mv2 ~s ~s" (f-name obj) newName)
    (tlog :debug "mv2: chmod= ~s" (f-perms obj))
    (unless (f-prevName obj)
      (tlog :debug "setting previous name to ~s" (f-name obj))
      (setf (f-prevName obj) (f-name obj))); if moved for the first time
    (unless (equal nd (f-name p)); if moved to another directory
      (setf (c-children p) (remove obj (c-children p) :count 1)); first divorse with  ex-parent directory
      (tlog :debug "parent name is ~s and has a DB-name ~s" nd (f-name np))
      (pushnew obj (c-children np)) ; then marry the new one
      (tlog :debug "changing parent name to ~s" (f-name np))
      (setf (f-parent obj) np)
      (correctName obj))))


;; note that it is possible that once upon a time a file existed, then it was erased,
;; and a then directory with the same name appeared
;; Warning: watch should not be applied to root dirs
(defun watch (objType fullName &key (erased 'nil) (modified 'nil) (doNotChmod 'nil))
  "(if it deserves,) add a file or a directory to the *allFiles* database"
  (let ((inDB (DBentry fullName :type objType)) (emm (or erased modified doNotChmod)))
    (tlog :debug "watch function received ~s" fullName)
    (tlog :debug "EMM=~b" emm)
    (if inDB
	(progn
	  (when erased   (setf (f-erased   inDB) t))
	  (when modified (setf (f-modified inDB) t)))
	(when ; if this is a newbie: check if it deserves our royal attention
	    (ecase objType
	      (file (importantP fullName))
	      (catalog (not (junkP fullName))))
	  (let* ((parent (DBentry (dirname fullName) :type 'catalog))
		 (newDBelement (make-instance objType
					      :name fullName
					      :parent parent
					      :group (unless emm (fileGroup fullName))
					      :date  (unless emm (fileDate fullName))
					      :perms (unless emm (fileMod fullName))
					      :modified modified)))
	    (pushnew newDBelement *allFiles* :test 'filesEqual)
	    (pushnew newDBelement (c-children parent) :test 'filesEqual)
	    (tlog :info "added file or directory ~s to the database" fullName)
	    (find newDBelement *allFiles* :test 'filesEqual))))))
;; ситуации вызова watch:
;; 1. при инициации добавляем каталоги из inotify-списка
;; 2. при созадании нового каталога или файла
;; 3. при удалении каталога или файла
;; 4. при изменении разрешений или даты
;; (неясно) При переименовании или перемещении каталога или файла

(defun sync ()
  (with-open-file (save localScript :direction :output :if-exists :supersede); ~/.esy/local.sh
    ;; (tlog :debug "writing shell scripts in ~~/.esy")
    (format save "~%# generated ~a by esy on ~s~%echo \"`date '+%Y-%m-%d\ %H:%M:%S'` @ `hostname` = `hostname -I`\" > ~~/.esy/host-id.dat~%timeout 5 emacsclient -e \"(save-some-buffers t)\"~%fetchmail --quit~%tar jcfv ~~/.esy/$(hostname).tbz ~~/.esy/host-id.dat ~~/.esy/remote.sh " (strDate) hostname)
    (mapcar #'(lambda (x) (format save " ~s" (f-name x))) ; these files will be saved into the tar archive
	    (remove-if-not #'(lambda (y) (and (f-modified y) (not (f-erased y)))) (selectFiles *allFiles*)))
    (format save "~%rm ~~/.esy/host-id.dat~%# end~%"))
  (with-open-file (script remoteScript :direction :output :if-exists :supersede)
    (format script "~%# generated ~a by esy on ~s~%# To be excecuted on remote host~%" (strDate) hostname)
    (format script "# tar xjfv --restrict --show-transformed --strip-components=3 ~~/.esy/~a.tbz -C ~~ ~%if [[ $(hostname) == ~s ]] ; then exit ; fi~%" hostname hostname)
    (format script "# moving directories:~%")
    (mapcar #'(lambda (x) (format script "mv -i ~s ~s~%" (f-prevName x) (f-name x)))
	    (remove-if-not #'(lambda (x) (f-prevName x)) (selectDirs *allFiles*)))
    (format script "# moving files:~%")
    (mapcar #'(lambda (x) (format script "mv -i ~s ~s~%" (f-prevName x) (f-name x)))
	    (remove-if-not #'(lambda (x) (f-prevName x)) (selectFiles *allFiles*)))
    (format script "# deleting files an directories:~%")
    (mapcar #'(lambda (x) (format script "rm -ri ~s~%" (f-name x)))
	    (remove-if-not #'(lambda (x) (f-erased x)) *allFiles*))
    (format script "# adjusting ownership (GIDs might be different on different hosts):~%")
    (mapcar #'(lambda (x) (format script "chgrp ~a ~s~%" (f-group x) (f-name x)))
	    (remove-if-not #'(lambda (x) (f-group x)) *allFiles*))
    (format script "# adjusting dates:~%")
    (mapcar #'(lambda (x) (format script "touch -d ~s ~s~%" (f-date x) (f-name x)))
	    (remove-if-not #'(lambda (x) (f-date x)) *allFiles*))
    (format script "# adjusting permissions:~%")
    (mapcar #'(lambda (x) (format script "chmod ~a ~s~%" (f-perms x) (f-name x)))
	    (remove-if-not #'(lambda (x) (f-perms x)) *allFiles*))))

(defun modAsteriscs (str) "transforms * into .*"
  (concatenate 'string
	       (flatten (loop for x across str collect
			     (if (equal x #\*)
				 (list #\. x)
				 x)))))
(defun shellToRegEx (shellPattern) "transforms shell patterns into regex for the cl-ppcre package"
       (when shellPattern
	 (let ((pat (if (typep shellPattern 'string) (list shellPattern) shellPattern)))
	 (modAsteriscs
	  (escapeString
	   (subseq
	    (apply 'concatenate
		   (cons 'string
			 (mapcar #'(lambda (x) (concatenate  'string "|" x "$")) pat)))
	    1) :escapeSymbols '(#\.))))))
;; test: (shellToRegEx '("*.tex"))

(defun WCorHashToRegEx (mixedList); mixedList is a mixture of strings and hashes
  "translates wildcards and name-hash-entries into patterns"
  (cons
   (shellToRegEx (remove-if-not #'(lambda (x) (typep x 'string)) mixedList))
   (mapcar #'(lambda (hts)
	       (if (nth-value 1 (gethash 'pattern hts))
		   (progn ; if "pattern" field exists, "name" is ignored
		     (setf (gethash 'name hts) (gethash 'pattern hts))
		     (remhash 'pattern hts))
		   (progn
		     (maphash #'(lambda (key val); otherwise we transform wildcard to pattern
				  (when (equal key 'name)
				    (setf (gethash key hts) (shellToRegEx val))))
			      hts)
		     hts)))
	   (remove-if-not #'(lambda (x) (typep x 'hash-table)) mixedList))))
;; test 1: (WCorHashToRegEx '("*.txt" "*.org" "*.tex" "*.lisp" "*.c" "*.sed"))

(defun parseNumberTimesUnit (str &key
	(modifiers '(("min" 60) ("hours?" 3600) ("days?" 86400) ("weeks?" 604800) ("months?" 18144000) ("years?" 6622560000) ("k" 1024) ("m" 1048576)))
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
(defun Fold (f x yList) "A Mathematica (R) Fold function"
       (let ((result x))
	 (loop for y in yList do
	      (setf result (funcall f result y)))
	 result)); semi-tested 2016-07-08

(defun recursive-subst (substRules object) "transforms object according to substRules"
       (Fold
	#'(lambda (structure rule)
	    (mapAll #'(lambda (obj) (if (equal (car rule) obj) (second rule) obj)) structure))
	object substRules)); tested 2016-07-08

(defun mps (str1 str2); arguments might be path names…
  (merge-pathnames (escapeString (namestring str1)) (escapeString (namestring str2))))

(defun errExit (message)
  (tlog :error message) ;(flush-log)
  (sleep 1)
  (exit))