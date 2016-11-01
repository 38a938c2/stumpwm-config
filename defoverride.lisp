(defun focus-all (win)
  "Focus the window, frame, group and screen belonging to WIN. Raise
  the window in it's frame."
  (when win
    (unmap-message-window (window-screen win))
    (switch-to-screen (window-screen win))
    (move-window-to-group win (current-group))
    (group-focus-window (window-group win) win)))
