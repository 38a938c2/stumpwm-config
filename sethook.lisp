(in-package :stumpwm)

(setf *urgent-window-hook* '())
(add-hook *urgent-window-hook* 'urgent-window-match-tags)

(setf *focus-window-hook* '())
(add-hook *focus-window-hook* 'remember-focus-window-hook)
(add-hook *focus-window-hook* 'default-tags-hook)
(add-hook *focus-window-hook* 'raise-top-windows-hook)
(add-hook *focus-window-hook* 'force-window-layout-hook)

(setf *new-window-hook* '())
(add-hook *new-window-hook* 'default-tags-hook)
(add-hook *new-window-hook* 'fullscreen-hook)
(add-hook *new-window-hook* 'default-layout-hook)

(setf *split-frame-hook* '())
(add-hook *split-frame-hook* 'frame-split-tagging-hook)

(setf *focus-group-hook* '())
(add-hook *focus-group-hook* 'echo-switch-group-hook)

(setf *start-hook* '())
(add-hook *start-hook* 'place-my-status-windows)
(add-hook *start-hook* 'reference-frame)
(add-hook *start-hook* 'do-status-split)
(add-hook *start-hook* 'modeline-killer-start-hook)

(add-hook *raise-request-hook* 'raise-request-match-tags)

(add-hook *property-notify-hook* 'early-window-name-update)
(add-hook *property-notify-hook* 'report-windows)
(add-hook *raise-request-hook* 'report-windows)
(add-hook *new-window-hook* 'report-windows)
(add-hook *destroy-window-hook* 'report-windows)
