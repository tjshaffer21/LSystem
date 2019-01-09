(asdf:defsystem #:lsystem
  :description "Implementation of LSystem"
  :author "Thomas Shaffer <tjshffer21@gmail.com>"
  :license "MIT"
  :version "2.0.0"
  :serial t
  :depends-on (:alexandria
               :cl-yaml
               :cl-glfw3
               :cl-opengl
               :iterate)
  :components ((:file "src/package")
               (:file "src/error")
               (:file "src/lsystem")
               (:file "src/gl")))
