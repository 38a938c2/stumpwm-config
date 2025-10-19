(in-package :stumpwm)

(defun echo-focus-window-hook (new old)
  (message "~a" new))

(defun echo-urgent-window-hook (target)
  (message "~a" target)
  )

(defun raise-urgent-window-hook (target)
  (when (should-be-raised target)
    (let*
      ((group (find-group (current-screen) "Default")))
      (gselect group)
      (move-window-to-group target group)
      (gselect group)
      (pull-w target)
      (gselect group)
      (group-focus-window group target)
      (gselect group)
      )))

(defun urgent-window-match-tags (window)
  (cond ((tagged-p window "RAISE-URGENT")
         (pull-w window)
         (really-raise-window))
        ((tagged-p window "NOTIFY-URGENT")
         (alexandria:write-string-into-file
           (window-title window)
           (format nil "~a/.breaking-news/za-urgent-windows"
                   (uiop:getenv "HOME"))
           :if-exists :append
           :if-does-not-exist :create))))

(defun remember-focus-window-hook (new old)
  (setf *globally-previous* *globally-current*)
  (when new (setf *globally-current* new)))

(defun click-conversation-focus-window-hook (new old)
  (when (or
	  (window-matches-properties-p new :class "Carrier" :role "conversation")
	  (window-matches-properties-p new :class "Pidgin" :role "conversation")
	  )
    (ratwarp 50 760)
    (ratclick 1) ; FIXME: it doesn't work
    (run-shell-command "echo -e 'ButtonPress 1\\nButtonRelease 1' | xmacroplay $DISPLAY")
    ))

(defun renumbering-start-hook ()
  (message "Starting renumbering hook")

  (place-existing-windows)

  (gselect (find-group (current-screen) "Net"))
  (net-window-sort)

  (switch-to-group (find-group (current-screen) "Base"))
  (repack-window-numbers)
  )

(defun modeline-start-hook ()
  (message "Setting up modeline")
  (let ((group1 (find-if (lambda (x) (= (group-number x) 1)) (screen-groups (current-screen)))))
    (gselect group1))
  (let ((window (global-matching-window :title "XWatchSystem")))
    (if window
      (progn
	(move-window-to-group window (current-group))
	(group-focus-window (current-group) window)
	(really-raise-window window)
	(my-mode-line :window window)))))

(defun window-set-start-hook ()
  (message "Restoring window set")
  (pull+push "IN-CURRENT-GROUP")
  (number-by-tags))

(defun echo-switch-group-hook (g1 g2)
  (format *error-output* "~s ~s" g1 g2))

(defun modeline-killer-start-hook ()
  (enable-mode-line (current-screen) (current-head) nil)
  )

(defun raise-top-windows-hook (new old)
  (declare (ignorable new) (ignorable old))
  (loop for x in *windows-on-top*
	with bad = nil
	do
	(or
	  (ignore-errors (progn (pull-w x) (raise-window x) t))
	  (push x bad)
	  )
	finally (setf *windows-on-top* (set-difference *windows-on-top* bad))
	)
  )

(defun default-tags-hook (&rest args)
  (ignore-errors (default-tags)))

(defun fullscreen-hook (new)
  (when (find "mode/fullscreen" (deftags new) :test 'equalp)
    (setf (window-fullscreen new) t)))

(defun activate-strictly-urgent (&rest args)
  "Choose a window that should always be active but lost focus"
  (or
    (act-on-matching-windows (x :screen) 
      (ignore-errors (titled-p x "rofi"))
      (pull-w x) (pull-window x) (really-raise-window x))
    (act-on-matching-windows (x :screen) 
      (ignore-errors (titled-p x "dmenu"))
      (pull-w x) (pull-window x) (really-raise-window x))
    ))

(defun raise-request-match-tags (window)
  (cond ((tagged-p window "IGNORE-RAISE")
         (setf *raise-request-deny* t))
        ((tagged-p window "NOTIFY-RAISE")
         (alexandria:write-string-into-file
           (window-title window)
           (format nil "~a/.breaking-news/za-raised-windows"
                   (uiop:getenv "HOME"))
           :if-exists :append
           :if-does-not-exist :create)
         (setf *raise-request-deny* t))))

(defparameter *window-reporting* ())

(defun report-windows-now (&rest args)
  (declare (ignore args))
  (let* ((reports 
           (remove
             nil
             (act-on-matching-windows
               (w :screen) t
               (loop for f in *window-reporting*
                     for r := (ignore-errors (funcall f w))
                     when r return r))))
         (target
           (format nil "~a/.breaking-news/zb-window-reports"
                   (uiop:getenv "HOME"))))
    (cond
      (reports
        (alexandria:write-string-into-file
          (format
            nil "~{~a~%~}"
            reports)
          target
          :if-exists :supersede
          :if-does-not-exist :create))
      ((probe-file target) (delete-file target))
      (t nil))))

(defun report-windows (&rest args)
  (unless (find "Delayed window reporter" (bordeaux-threads:all-threads)
                :key 'bordeaux-threads:thread-name :test 'equal)
    (bordeaux-threads:make-thread
      (lambda () (sleep 1) (apply 'report-windows-now args))
      :name "Delayed window reporter")))

(defparameter *current-window-rename* nil)

(defun early-window-name-update (w atom)
  (if (eq atom :wm_name)
    (ignore-errors
      (setf *current-window-rename*
            (list w (window-title w) (xwin-name (window-xwin w))))
      (setf (window-title w) (xwin-name (window-xwin w))))
    (setf *current-window-rename* nil)))

(defun lock-window-title (w atom)
  (when (eq atom :wm_name)
    (let* ((name (xwin-name (window-xwin w))))
      (when (and (> (length name) 0)
                 (equal (subseq name 0 2) "≝🔐"))
        (setf (window-user-title w) (subseq name 2))))))

(defun window-title-tag (w atom)
  (when (eq atom :wm_name)
    (let* ((name (xwin-name (window-xwin w))))
      (when (and (> (length name) 0)
                 (equal (subseq name 0 2) "≝🏷"))
        (tag-window (subseq name 2) w)))))

(defun window-title-nofullscreen (w &optional atom)
  (when (or (not atom) (eq atom :wm_name))
    (let* ((name (xwin-name (window-xwin w))))
      (when (and (> (length name) 0)
                 (or
                   (equal (subseq name (- (length name) 2)) "❌⤢")
                   (equal (subseq name 0 2) "❌⤢")))
        (deactivate-fullscreen w)))))

(defun start-stumpwm-socket-start-hook ()
  (ignore-errors (start-stumpwm-socket)))

(defun minor-x-config-tools-start-hook ()
  (uiop:launch-program "echo -n ' ' | xsel -i")
  (uiop:launch-program `("xsetroot" "-solid" "#99bb66")))
