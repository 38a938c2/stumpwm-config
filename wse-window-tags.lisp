(in-package :stumpwm)

(defun push-w (&optional w) 
  "Move the specified window (or the current one) to a special hidden group"
  (unless (find-group (current-screen) ".tag-store")
    (gnewbg ".tag-store"))
  (move-windows-to-group (list (or w (curent-window))) ".tag-store"))

(defun tagged-p (w tag)
  "Check whether the window w has the tag;
  T is a special tag ascribed to all windows"
  (or (equal tag "T") (find tag (window-tags w) :test 'equalp)))

(defun tagged-any-p (w argtags) 
  "Check whether the window w has any of the tags specified by argtags"
  (let* 
    ((tag (if (stringp argtags) 
	    (remove "" (cl-ppcre:split " " (string-upcase argtags))
		    :test 'equalp)
	    (mapcar 'string-upcase argtags))))
    (intersection tag (cons "T" (window-tags w)) :test 'equalp)))

(defun tag-re-p (w tre) 
  "Check whether the window w has a tag matching the regular expression tre"
  (find-if (lambda (tag) (cl-ppcre:scan tre tag)) 
	   (cons "T" (window-tags w))))
