(defun focus-all (win)
  "Focus the window, frame, group and screen belonging to WIN. Raise
  the window in it's frame."
  (when win
    (unmap-message-window (window-screen win))
    (switch-to-screen (window-screen win))
    (move-window-to-group win (current-group))
    (group-focus-window (window-group win) win)))

(unless (boundp '*raise-request-hook*)
  (defvar *raise-request-hook* ())
  (defvar *raise-request-deny* nil)
  (defvar *maybe-raise-window-function*
    (function maybe-raise-window))
  (defun maybe-raise-window (window)
    (let* ((*raise-request-deny* nil))
      (run-hook-with-args *raise-request-hook* window)
      (unless *raise-request-deny*
        (funcall *maybe-raise-window-function*)))))
