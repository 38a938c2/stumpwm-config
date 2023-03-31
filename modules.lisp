(in-package :stumpwm)

(loop for x in `(:clx-truetype :xkeyboard :xembed)
      do (require x))

(defparameter *local-module-dir*  "/home/repos/stumpwm-contrib/")
(set-module-dir *local-module-dir*)
(load-module "windowtags")
(load-module "ttf-fonts")
(use-package :windowtags)
(import 'windowtags::select-by-tags)


(loop for x in `(:clx-truetype :xkeyboard :xembed :windowtags :ttf-fonts)
      do (require x))
