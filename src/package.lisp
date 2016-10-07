;;;; package.lisp

(defpackage #:lsystem
  (:use #:cl)
  (:export :main
           :create-lsystem
           :substitute-for
           :variables
           :constants
           :rules
           :lsystem
           :len))
