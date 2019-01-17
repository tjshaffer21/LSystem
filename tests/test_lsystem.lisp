(in-package #:lsystem.tests)

(defun test-to-string (lsys)
  (format nil "~{~A~}" (iterate:iter
                      (iterate:for i iterate::in lsys)
                      (iterate:collecting
                        (code-char i)))))

(lisp-unit:define-test test-count
    (let ((system (lsystem:create-lsystem-from-file "data/tree1.yaml")))
        (lisp-unit:assert-equal 1 (lsystem:lsystem-count system :all))
        (lisp-unit:assert-equal 1 (lsystem:lsystem-count system :nonterminal))
        (lisp-unit:assert-equal 0 (lsystem:lsystem-count system :termial))

        (setf system (lsystem:substitution system 1))
        (lisp-unit:assert-equal 11 (lsystem:lsystem-count system :all))
        (lisp-unit:assert-equal 5 (lsystem:lsystem-count system :nonterminal))
        (lisp-unit:assert-equal 6 (lsystem:lsystem-count system :terminal))))

;; Single nonterminal, multiple terminals.
(lisp-unit:define-test test-tree1
  (let ((system (lsystem:create-lsystem-from-file "data/tree1.yaml")))
    (lisp-unit:assert-equal "F"
        (test-to-string (lsystem:lsystem-state system)))
    (setf system (lsystem:substitution system 1))
    (lisp-unit:assert-equal "F[+F]F[-F]F"
        (test-to-string (lsystem:lsystem-state system)))
    (setf system (lsystem:substitution system 1))
    (lisp-unit:assert-equal
        "F[+F]F[-F]F[+F[+F]F[-F]F]F[+F]F[-F]F[-F[+F]F[-F]F]F[+F]F[-F]F"
        (test-to-string (lsystem:lsystem-state system)))))

(lisp-unit:define-test test-tree3
  (let ((system (lsystem:create-lsystem-from-file "data/tree3.yaml")))
    (lisp-unit:assert-equal "F"
        (test-to-string (lsystem:lsystem-state system)))
    (setf system (lsystem:substitution system 1))
    (lisp-unit:assert-equal "FF+[+F-F-F]-[-F+F+F]"
        (test-to-string (lsystem:lsystem-state system)))
    (setf system (lsystem:substitution system 1))
    (lisp-unit:assert-equal
        "FF+[+F-F-F]-[-F+F+F]FF+[+F-F-F]-[-F+F+F]+[+FF+[+F-F-F]-[-F+F+F]-FF+[+F-F-F]-[-F+F+F]-FF+[+F-F-F]-[-F+F+F]]-[-FF+[+F-F-F]-[-F+F+F]+FF+[+F-F-F]-[-F+F+F]+FF+[+F-F-F]-[-F+F+F]]"
        (test-to-string (lsystem:lsystem-state system)))))

;; Two nonterminals, multiple terminals.
(lisp-unit:define-test test-tree2
  (let ((system (lsystem:create-lsystem-from-file "data/tree2.yaml")))
    (lisp-unit:assert-equal "X"
        (test-to-string (lsystem:lsystem-state system)))
    (setf system (lsystem:substitution system))
    (lisp-unit:assert-equal "F[+X][-X]FX"
        (test-to-string (lsystem:lsystem-state system)))
    (setf system (lsystem:substitution system))
    (lisp-unit:assert-equal "FF[+F[+X][-X]FX][-F[+X][-X]FX]FFF[+X][-X]FX"
        (test-to-string (lsystem:lsystem-state system)))))

(lisp-unit:define-test test-history
  (let ((system (lsystem:create-lsystem-from-file "data/tree2.yaml")))
    (lisp-unit:assert-equal "X"
      (test-to-string (lsystem:lsystem-state system)))
    (setf system (lsystem:substitution system 2))
    (lisp-unit:assert-equal "F[+X][-X]FX"
      (test-to-string (lsystem:lsystem-state system 1)))))