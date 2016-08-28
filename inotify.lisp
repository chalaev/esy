;; Time-stamp: <2016-07-24 14:27 EDT by Oleg SHALAEV http://chalaev.com >

(defun act-on-inotify (events)
  (progn (tlog :debug "received ~d events" (length events))
  (dolist (te events)
    (let* ((dir (escapeString (namestring (inotify:watch-pathname (inotify:event-watch te))))); имя каталога
	   (file (inotify:event-name te)); имя изменённого объекта (файла или каталога); ≠nil и в случае, если создан/изменён каталог.
	   (whatsUp (inotify:event-mask te))
	   (moveCookie (inotify:event-cookie te)); nonzero for rename/move operations -- must be the same for connected move-from and move-to!
	   (thisMovedFrom (find moveCookie movedFrom :test #'(lambda (x y) (eql x (second y))))); almost always empty
	   (fullName (if file (namestring (mps file dir)) dir)) printName)
      (unless (zerop (logand whatsUp inotify:in-isDIR))
	(setf fullName (fixDir fullName)))
      (setf printName (dTildas fullName))
      (tlog :debug "event ~B occured with ~s" whatsUp printName)
      (setf movedFrom (delete thisMovedFrom movedFrom))
      (case whatsUp
	(#.inotify:in-delete-self (tlog :debug "(ignored) deleted observed  dir ~s" printName))
	(#.rmdir (rm (DBentry fullName :type 'catalog)))
	(#.inotify:in-create
	 (case (osicat:file-kind fullName); for now links are not controlled
	   (#.:directory    (watch 'catalog fullName))
	   (#.:regular-file (watch 'file    fullName))))
	(#.inotify:in-modify (watch 'file fullName :modified t))
	((#.inotify:in-moved-from #.mvDirFrom) (push (list fullName moveCookie (get-internal-real-time)) movedFrom))
	(#.inotify:in-moved-to
	 (let ((inDB (DBentry (first thisMovedFrom))))
	   (unless inDB (setf inDB (watch 'file (first thisMovedFrom) :doNotChmod t)))
	   (mv inDB fullName)))
	(#.mvDirTo
	 (if (DBentry (dirName (first thisMovedFrom)) :type 'catalog) ; if moved from a monitored directory
	     (mv (DBentry (first thisMovedFrom) :type 'catalog) fullName :type 'catalog :movedFromElseWhere 'nil)
	     (mv (DBentry (first thisMovedFrom) :type 'catalog) fullName :type 'catalog :movedFromElseWhere 't)))
	;; (#.inotify:in-move-self (tlog :info "an observed directory is moved")) ; decided not to monitor this event
	(#.inotify:in-delete (rm (watch 'file fullName :erased t)))
	(#.mkdir (let ((fullName (fixDir fullName)))
		   (watch 'catalog fullName) (push fullName newDirsToWatch)))
	(#.inotify:in-attrib ; chgrp or touch or chmod on file
	 (tlog :debug "chgrp or chmod file ~s" printName)
	 (update-attrs (watch 'file fullName)))
	(#.chmodDir ; chgrp or chmod on dir
	 (tlog :debug "chgrp or chmod dir ~s" printName)
	 (update-attrs (watch 'catalog fullName)))
	(#.inotify:in-ignored (tlog :info "(presumably due to its removal) stopped monitoring directory ~s" printName))
	(otherwise (tlog :error "some strange unmatched event ~B occured!" whatsUp)))))))
