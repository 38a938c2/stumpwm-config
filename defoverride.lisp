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
        (funcall *maybe-raise-window-function* window)))))

;; This functionality is used once, and I do not want it anyway
(defun populate-frames (group))
;; This is used more, and I still do not want it
(defun choose-new-frame-window (frame group))

(defmethod group-move-request ((group tile-group) (window tile-window) x y (relative-to (eql :root)))
  (when *honor-window-moves*
    (dformat 3 "Window requested new position ~D,~D relative to ~S~%" x y relative-to)
    (let* ((pos  (list x y))
           (frame (apply #'find-frame group pos)))
      (when frame
        (pull-window window frame)))))
