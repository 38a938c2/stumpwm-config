(in-package :stumpwm)

(defvar *local-module-dir*  "/home/repos/stumpwm-contrib/")
(set-module-dir *local-module-dir*)
(load-module "windowtags")
(load-module "ttf-fonts")
(use-package :windowtags)
(import 'windowtags::select-by-tags)

