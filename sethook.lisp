(setf *urgent-window-hook* '())
(add-hook *urgent-window-hook* 'raise-urgent-window-hook)

(setf *focus-window-hook* '())
(add-hook *focus-window-hook* 'remember-focus-window-hook)
(add-hook *focus-window-hook* 'default-tags-hook)
(add-hook *focus-window-hook* 'raise-top-windows-hook)
(add-hook *focus-window-hook* 'force-window-layout-hook)

(setf *new-window-hook* '())
(add-hook *new-window-hook* 'default-tags-hook)
(add-hook *new-window-hook* 'default-layout-hook)

(setf *split-frame-hook* '())
(add-hook *split-frame-hook* 'frame-split-tagging-hook)

(setf *focus-group-hook* '())
(add-hook *focus-group-hook* 'echo-switch-group-hook)

(setf *start-hook* '())
(add-hook *start-hook* 'fclear)
(add-hook *start-hook* 'modeline-killer-start-hook)
