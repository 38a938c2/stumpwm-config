(in-package :stumpwm)

(loop for x in `(
		 :clx-truetype 
		 :xkeyboard 
		 :xembed
		 )
      do (require x))

(load-rcpart "window-tags")
(use-package :windowtags)

(load-rcpart "ttf-fonts")
