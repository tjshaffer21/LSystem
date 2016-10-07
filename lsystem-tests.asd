;;;; lsysem-tests.asd
(asdf:defsystem #:lsystem-tests
  :description "Unit Tests for LSystem."
  :author "Thomas Shaffer <tjshaffer21@gmail.com>"
  :depends-on (:iterate
                :cl-utilities
                :lisp-unit
                :lsystem)
  :serial t
  :components((:file "tests/package")
              (:file "tests/test_lsystem")))
