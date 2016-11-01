(in-package :stumpwm)

(defun in-frame-tagged-p (w name)
  "Check whether the window w lies in a frame tagged with the tag name"
  (find name (frame-tags (window-frame w)) :test 'equalp))

(defun in-frame-tag-re-p (w tre)
  "Check whether the window w lies in a frame tagged with a tag matching
  the regular expression tre"
  (find-if (lambda (x) (cl-ppcre:scan tre x)) (frame-tags (window-frame w))))

(defun to-tagged-frame (w name &optional group)
  "Move the window w to the first frame tagged with the tag name in the
  specified or the current group"
  (let*
    ((frame (first-frame-by-tag name group)))
    (when frame (pull-window w frame))))

(defun to-re-tagged-frame (w tre &optional group)
  "Move the window w to the first frame tagged with a tag matching tre in the
  specified or the current group"
  (let*
    ((frame (first-frame-by-tag-re tre group)))
    (when frame (pull-window w frame))))
