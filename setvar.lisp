(in-package :stumpwm)

(setf *default-package* :stumpwm)

(setf *debug-level* 0)
(sb-ext:disable-debugger)

(defvar *fonts-cached* nil)
(unless *fonts-cached*
  (setf xft:*font-dirs* (list "/run/current-system/sw/share/fonts/truetype/"))
  (xft:cache-fonts)
  (setf *fonts-cached* t)
  )

(or
  (ignore-errors
    (progn
      (set-font (make-instance 'xft:font :family "DejaVu Sans Mono" :subfamily "Book" :size 8))
      t))
  (set-font "-*-*-r-*-20-*"))

(set-fg-color *fg-color*)
(set-bg-color *bg-color*)
(set-border-color *border-color*)


(setf *min-frame-height* 25)
(setf *window-format* "%m%n %s [ %70t ] (%30c | %30i | %30r)")

(setf (head-mode-line (current-head)) nil)
(enable-mode-line (current-screen) (current-head) nil)

(setf *normal-border-width* 0)

(setf *input-window-gravity* :top-right)

(setf *top-level-error-action* :abort)

(setenv "SHELL" "/run/current-system/sw/bin/zsh")
(setenv "SBCL_HOME" "")
