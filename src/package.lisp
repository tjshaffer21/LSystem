(defpackage #:lsystem
  (:use #:cl)
  (:export :main
           :create-lsystem-from-file
           :do-substitution-time
           :get-lsystem-current
           :get-lsystem-at
           :rules
           :substitution
           :token-vtype
           :token-value))
