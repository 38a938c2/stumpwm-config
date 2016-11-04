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

(require :xkeyboard)
(xlib::initialize-extensions *display*)
(xlib:enable-xkeyboard *display*)
(defun force-window-layout-hook (new &optional old)
  (when
    (and old (find "xkbgr/!" (window-tags old) :test 'equalp))
    (set-window-layout 
      (xlib:device-state-group (xlib:get-state *display*))
      :window old :keep-sticky t))
  (let*
    (
     (tags (window-tags new))
     (grtag (find-if (lambda (x) (cl-ppcre:scan "^XKBGR/[0-9]+$" x)) tags))
     (grtag (when grtag (cl-ppcre:regex-replace "^XKBGR/" grtag "")))
     (gr (ignore-errors (parse-integer grtag)))
     )
    (when gr (xlib:lock-group *display* :group gr))
    )
  )

(defun default-layout-hook (new &rest args)
  (let
    ((gr 
       (cond
         ((find (window-title new) '("rofi") :test 'equalp) 0)
         (t nil))))
    (when gr (xlib:lock-group *display* :group gr))))

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
