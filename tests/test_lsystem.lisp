(in-package #:lsystem.tests)

;; Single nonterminal, multiple terminals.
(lisp-unit:define-test test-tree1
  (let ((system (lsystem:create-lsystem-from-file "data/tree1.yaml")))
    (lisp-unit:assert-equal "F" (lsystem:get-lsystem-current system))
    (setf system (lsystem:substitution system))
    (lisp-unit:assert-equal "F[+F]F[-F]F" (lsystem:get-lsystem-current system))
    (setf system (lsystem:substitution system))
    (lisp-unit:assert-equal
        "F[+F]F[-F]F[+F[+F]F[-F]F]F[+F]F[-F]F[-F[+F]F[-F]F]F[+F]F[-F]F"
        (lsystem:get-lsystem-current system))))

(lisp-unit:define-test test-tree3
  (let ((system (lsystem:create-lsystem-from-file "data/tree3.yaml")))
    (lisp-unit:assert-equal "F" (lsystem:get-lsystem-current system))
    (setf system (lsystem:substitution system))
    (lisp-unit:assert-equal "FF+[+F-F-F]-[-F+F+F]"
        (lsystem:get-lsystem-current system))
    (setf system (lsystem:substitution system))
    (lisp-unit:assert-equal
        "FF+[+F-F-F]-[-F+F+F]FF+[+F-F-F]-[-F+F+F]+[+FF+[+F-F-F]-[-F+F+F]-FF+[+F-F-F]-[-F+F+F]-FF+[+F-F-F]-[-F+F+F]]-[-FF+[+F-F-F]-[-F+F+F]+FF+[+F-F-F]-[-F+F+F]+FF+[+F-F-F]-[-F+F+F]]"
        (lsystem:get-lsystem-current system))))

;; Two nonterminals, multiple terminals.
(lisp-unit:define-test test-tree2
  (let ((system (lsystem:create-lsystem-from-file "data/tree2.yaml")))
    (lisp-unit:assert-equal "X" (lsystem:get-lsystem-current system))
    (setf system (lsystem:substitution system))
    (lisp-unit:assert-equal "F[+X][-X]FX" (lsystem:get-lsystem-current system))
    (setf system (lsystem:substitution system))
    (lisp-unit:assert-equal "FF[+F[+X][-X]FX][-F[+X][-X]FX]FFF[+X][-X]FX"
        (lsystem:get-lsystem-current system))))