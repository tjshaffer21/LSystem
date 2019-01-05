(defpackage #:lsystem
  (:use #:cl)
  (:export :main
           :create-lsystem-from-file
           :do-substitution-time
           :get-lsystem-current
           :nonterminals
           :rules
           :substitution
           :terminals))
