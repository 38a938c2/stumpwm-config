(in-package :stumpwm)

(defmacro define-frame-preference-eval (&rest x)
  `(define-frame-preference
     ,@(mapcar (lambda (y) (eval y)) x)))

(clear-window-placement-rules)

(define-frame-preference-eval "Base"
  (lock-rule-by-class "zsh")
  (lock-rule-by-class "bash")
  (lock-rule-by-class "sh")
  )

(define-frame-preference-eval "Net"
  (lock-rule-by-class "Carrier")
  (lock-rule-by-class "Pidgin")
  (lock-rule-by-class "Thunderbird-bin")
  (lock-rule-by-class "Minefield")
  )

(define-frame-preference-eval ".system"
  (lock-rule-by-title "Local IRC ghost")
  (lock-rule-by-title "Gateway6 monitoring")
  (lock-rule-by-title "WPA Supplicant")
  )

(define-frame-preference-eval "Administrating"
  (lock-rule-by-title "su shell")
  )

(clear-window-placement-rules)

