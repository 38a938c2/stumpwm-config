(in-package :stumpwm)

(defparameter *here*
  (cl-ppcre:regex-replace 
    "/[^/]*$" (namestring (truename 
                            (or
                              *compile-file-truename*
                              *load-truename*)))
    ""))
(defun load-rcpart (name)
  (load (concatenate 'string *here* "/" name ".lisp")))



(defun getenv (var-name) 
  (#+clisp ext:getenv
   #+sbcl  sb-posix:getenv
   #+ccl  ccl:getenv
   #+ecl  ext:getenv
   var-name
   ))

(defun setenv (var value)
  #+sbcl (sb-posix:setenv var value 1)
  )

(defparameter *HOME* (getenv "HOME"))

(defvar *langos* "")
(let ((langos (concatenate 'string *HOME* "/src/nix/lang-os/stumpwm")))
  (when (probe-file langos)
    (setf *langos* langos)))

; https://github.com/7c6f434c/lang-os
(defun load-rcpart-langos (name)
  (load (concatenate 'string *langos* "/" name ".lisp")))

(defun stumpwm::string-to-utf8 (string)
  (sb-ext:string-to-octets string :external-format :utf-8))

