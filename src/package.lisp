(defpackage #:lsystem
  (:use #:cl)
  (:export :main
           :create-lsystem-from-file
           :lsystem-count
           :lsystem-rule
           :lsystem-state
           :rules
           :substitution
           :token-of
           :token-type-of-p
           :token-vtype
           :token-value))
