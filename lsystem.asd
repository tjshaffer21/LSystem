;;;; lsystem.asd

(asdf:defsystem #:lsystem
  :description "Implementation of LSystem"
  :author "Thomas Shaffer <tjshffer21@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (:cl-utilities
               :cl-glfw3
               :cl-opengl
               :iterate)
  :components ((:file "src/package")
               (:file "src/translation")
               (:file "src/lsystem")))
