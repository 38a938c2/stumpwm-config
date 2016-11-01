;; Copyright 2011 Michael Raskin
;;
;; Maintainer: Michael Raskin
;;
;; This file is part of stumpwm.
;;
;; stumpwm is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; stumpwm is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307 USA

;; Frame tagging

(in-package :stumpwm)

(defvar *frame-tags* (make-hash-table))

(defcommand frame-tags (&optional (argframe nil)) ()
  "Show frame tags"
  (let* ((frame (or argframe (tile-group-current-frame 
			       (current-group))))
	 (tags (gethash frame *frame-tags*)))
    (if argframe tags (message "Tags: ~{~%~a~}" tags))))

(defun (setf frame-tags) 
  (argtags &optional (frame (tile-group-current-frame 
			      (current-group))))
  "Set frame tags"
  (let*
    ((tags (if (stringp argtags) 
	     (remove "" (cl-ppcre:split " " (string-upcase argtags))
		     :test 'equalp)
	     (mapcar 'string-upcase argtags))))
    (setf (gethash frame *frame-tags*) tags)))

(defun remove-frame-tags 
  (argtags &optional (frame (tile-group-current-frame 
			      (current-group))))
  "Remove frame tags specified by argtags from the specified frame (current
  frame by default)"
  (let*
    ((tags (if (stringp argtags) 
	     (remove "" (cl-ppcre:split " " (string-upcase argtags)) :test 'equal) 
	     (mapcar 'string-upcase argtags))))
    (setf (frame-tags frame) 
	  (remove-if (lambda (x) (find x tags :test 'equalp))
		     (frame-tags frame)))))

(defcommand 
  tag-frame (argtags &optional (frame (tile-group-current-frame 
					(current-group))))
  ((:rest "Tag to set: ") :rest)
  "Add a tag to the specified or the current frame"
  (let*
    ((tags 
       (if (stringp argtags) 
	 (remove "" (cl-ppcre:split " " (string-upcase argtags)) :test 'equal)
	 (mapcar 'string-upcase argtags))))
    (setf (frame-tags frame) (union tags (frame-tags frame) :test 'equalp))))

(defcommand 
  clear-all-frame-tags (&optional (frame (tile-group-current-frame 
					   (current-group)))) ()
  "Remove all tags from the specified or the current frame"
  (setf (frame-tags frame) nil))

(defun first-frame-by-tag (tag &optional group)
  "Find the first frame that has the specified tag in the specified or the
  current group"
  (loop for x in (group-frames (or group (current-group)))
	when (find (string-upcase tag) (frame-tags x) :test 'equal)
	return x))

(defun first-frame-by-tag-re (tag &optional (group (current-group)))
  "Find the first frame that has a tag matching the regular expression tag
  in the specified or the current group"
  (loop for x in (group-frames group)
	when (find (string-upcase tag) (frame-tags x) :test 
		   (lambda (x y) (cl-ppcre:Scan x y)))
	return x))

(defcommand focus-frame-by-tag (tag) ((:rest "Tag: "))
  "Focus the first frame having the specified tag"
  (let*
    ((frame (first-frame-by-tag tag)))
    (when frame (focus-frame (current-group) frame))))

(defcommand focus-frame-by-tag-re (tag) ((:rest "Tag: "))
  "Focus the first frame having a tag matching the regular expression tag"
  (let*
    ((frame (first-frame-by-tag-re tag)))
    (when frame (focus-frame (current-group) frame))))
