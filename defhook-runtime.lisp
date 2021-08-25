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
         (t 0))))
    (when gr (xlib:lock-group *display* :group gr))))


