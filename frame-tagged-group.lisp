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

;; Frame groups based on tagging

(in-package :stumpwm)

(defun frame-tagged-group (f)
  "Find the FTG of the frame f"
  (or
    (loop for x in (frame-tags f) when (cl-ppcre:scan "^TG/.*$" x) 
	  return x)
    "DEFAULT"))

(defun in-ftg-p (w &optional x)
  "Check whether the window w lies in the frame-tagged group with the
  tag x (defaults to the current ftg)"
  (equalp (frame-tagged-group (window-frame w)) 
	  (or x (frame-tagged-group 
		  (tile-group-current-frame (current-group))))))

(defcommand
  ftg-push-pull-tags (argtags) ((:rest "Tags: "))
  "Move away all windows from current frame group not with tags in argtags
  and pull into current frame all windows with tags amongs argtags placed 
  in other frame groups or in other groups"
  (let*
    ((tag (if (stringp argtags) 
	    (remove "" (cl-ppcre:split " " (string-upcase argtags))
		    :test 'equalp)
	    (mapcar 'string-upcase argtags)))
     (not-tag
       (mapcar 
	 (lambda (x) (subseq x 1))
	 (remove-if
	   (lambda (x) (not (equal (elt x 0) #\!)))
	   tag)))
    (ftg (frame-tagged-group (tile-group-current-frame (current-group)))))
    (fclear)
    (act-on-matching-windows
      (w :group) (and (in-ftg-p w ftg) 
		      (not (tagged-any-p w tag)))
      (push-w w))
    (act-on-matching-windows
      (w :screen) (and 
		    (not (grouped-p w))
		    (tagged-any-p w tag))
      (pull-w w))
    (act-on-matching-windows
      (w :screen) (and 
		    (not (in-ftg-p w ftg))
		    (tagged-any-p w tag)
		    (not (tagged-any-p w not-tag)))
      (pull-window w))))

(defcommand
  ftg-next-window () ()
  "Switch to next window in the current ftg"
  (focus-forward
    (current-group)
    (sort
      (act-on-matching-windows 
	(w :group)
	(in-ftg-p w (frame-tagged-group 
			   (tile-group-current-frame (current-group))))
	w)
      '< :key 'window-number)
    nil))

(defcommand 
  ftg-set-tags (Argtags) ((:rest "Tags: "))
  "A recommended handler of window tag choice for the current FTG"
  (when
    (equal (group-name (current-group)) ".tag-store")
    (gselect (find-group (current-screen) "Default")))
  (ftg-push-pull-tags (concatenate 'string argtags " !sticky"))
  (number-by-tags)
  (unless (current-window) (ftg-next-window))
  )

(defcommand
  (set-ftg tile-group) (name) ((:rest "Name: "))
  "Set tagged-group of current frame"
  (unless (cl-ppcre:scan " " name)
    (setf (frame-tags (tile-group-current-frame (current-group)))
	  (cons
	    (format nil "TG/~a" (string-upcase name))
	    (remove-if
	      (lambda (s) (cl-ppcre:scan "^TG/" s))
	      (frame-tags (tile-group-current-frame (current-group))))))))

(defun eat-frame (group eater food)
  "An implementation of remove-split that ensures that all the space from the 
  frame food goes to the frame eater"
  (dformat 7 "~%Eat frame? ~s:~%~s ~s~%@~%~s ~s~%" 
	  group eater food (frame-head group eater) (frame-head group food))
  (when 
    (equal (frame-head group eater) (frame-head group food))
    (dformat 9 "Frame tree was: ~s~%"
	    (tile-group-frame-tree group))
    (if (= (frame-y eater) (frame-y food))
      (progn 
	(setf (frame-width eater) (+ (frame-width eater) (frame-width food)))
	(when (> (frame-x eater) (frame-x food))
	  (setf (frame-x eater) (frame-x food)
		(frame-x food) (1+ (frame-x food))))
	(setf (frame-width food) 0))
      (progn
	(setf (frame-height eater) 
	      (+ (frame-height eater) (frame-height food)))
	(when (> (frame-y eater) (frame-y food))
	  (setf (frame-y eater) (frame-y food)
		(frame-y food) (1+ (frame-y food))))
	(setf (frame-height food) 0)))
    (dformat 9 "Frame tree is: ~s~%"
	    (tile-group-frame-tree group))
    (remove-split group food)))

(defun every-in-tree (p tr)
  "Check whether all the element of the tree tr satisfy the predicate p"
  (cond
    ((null tr) t)
    ((atom tr) (funcall p tr))
    (t (not (find-if (lambda (x) (not (every-in-tree p x))) tr)))))

(defun eat-ftg-siblings (&key (group (current-group)) 
			      (frame (tile-group-current-frame group)))
  "Make the specified or the current frame consume all the sibling frames 
  belonging to the same ftg; return whether any were found"
  (let*
    ((tr (tile-group-frame-tree group))
     (ftg (frame-tagged-group frame))
     (s tr)
     (d 0)
     (n (position frame s))
     (l (length s)))
    (loop while (not n)
	  do (progn
	       (setf s (next-sibling s frame))
	       (setf n (position frame s))
	       (setf l (length s))
	       (incf d)))
    (if (= d 0) nil
      (< 0 
	 (+
	   (if (> n 0)
	     (let*
	       ((food (nth (1- n) s)))
	       (if
		 (every-in-tree 
		   (lambda (x) (equalp (frame-tagged-group x) ftg))
		   food)
		 (progn 
		   (if (listp food)
		     (progn
		       (focus-frame group (tree-leaf food))
		       (ftg-only))
		     (eat-frame group frame food))
		   1)
		 0))
	     0)
	   (if (< n (1- l))
	     (let*
	       ((eater (nth (1+ n) s)))
	       (if
		 (every-in-tree 
		   (lambda (x) (equalp (frame-tagged-group x) ftg))
		   eater)
		 (progn 
		   (if (listp eater)
		     (progn
		       (focus-frame group (tree-leaf eater))
		       (ftg-only))
		     (eat-frame group eater frame))
		   1)
		 0))
	     0))))))

(defcommand ftg-only () ()
  "Make the current frame occupy the maximum rectangular part of the 
  current FTG."
  (loop while (eat-ftg-siblings)))

(defcommand 
  ftg-mark-windows (argtags) ((:rest "Tags: "))
  "Mark all windows in the frames of the current FTG with argtags"
  (act-on-matching-windows
    (w :group)
    (in-ftg-p w (frame-tagged-group 
		       (tile-group-current-frame (current-group))))
    (tag-window argtags w)))

(defcommand
  ftg-set-tag-re (re &optional keep-old) ((:rest "Tag pattern: "))
  "In current ftg there will be all windows with tags matching re"
  (fclear)
  (setf re (string-upcase re))
  (unless keep-old
    (act-on-matching-windows
      (w :group)
      (and
	(in-ftg-p w)
	(not (find-if (lambda (x) (cl-ppcre:scan re x)) (window-tags w))))
      (push-w w)))
  (act-on-matching-windows
    (w :screen)
    (and
      (not
	(and
	  (in-ftg-p w)
	  (grouped-p w)))
      (find-if (lambda (x) (cl-ppcre:scan re x)) (window-tags w)))
    (pull-w w))
  (act-on-matching-windows
    (w :group)
    (and
      (not (in-ftg-p w))
      (find-if (lambda (x) (cl-ppcre:scan re x)) (window-tags w)))
    (pull-window w)))

(defcommand 
  ftg-add-tag-re (re) ((:rest "Tag pattern: "))
  "Add all windows with tags matching re to current ftg"
  (ftg-set-tag-re re t))

(defun frame-split-tagging-hook (p f1 f2)
  "Put this into the frame-split-hook to make child frames inherit the tags of
  the parent frame on split"
  (setf (frame-tags f1) (frame-tags p))
  (setf (frame-tags f2) (frame-tags p)))

(defun ftg-windows ()
  "List of all windows in the current ftg"
  (act-on-matching-windows
    (w :group)
    (in-ftg-p w)
    w))
