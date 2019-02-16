(push
  (lambda (w)
    (when (and (window-title w)
               (tagged-p w "telegram")
               (eq w (first *current-window-rename*))
               (cl-ppcre:scan "[0-9]+ notifications?"
                              (second *current-window-rename*))
               (not (eq w (current-window))))
      (setf (window-title w)
            (concatenate 
              'string (third *current-window-rename*)
              ": "
              (second *current-window-rename*)))
      (format nil "Telegram: ~a"
              (cl-ppcre:regex-replace-all
                "[^0-9]" (second *current-window-rename*) ""))))
  *window-reporting*)
(push
  (lambda (w)
    (when (and (window-title w)
               (tagged-p w "telegram")
               (title-re-p w "[0-9]+ notifications?")
               (not (eq w (current-window))))
      (format nil "Telegram: ~a"
              (cl-ppcre:regex-replace-all
                "[^0-9]" (window-title w) ""))))
  *window-reporting*)
(push
  (lambda (w)
    (when (and (window-title w)
               (tagged-p w "telegram")
               (title-re-p w ": [0-9]+ notifications?")
               (eq w (current-window)))
      (setf (window-title w)
            (cl-ppcre:regex-replace-all
              ": [0-9]+ notifications?"
              (window-title w) ""))
      nil))
  *window-reporting*)
