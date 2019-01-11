(in-package #:lsystem.tests)

(defun test-to-string (lsys)
  (format nil "~{~A~}" (iterate:iter
                      (iterate:for i iterate::in lsys)
                      (iterate:collecting (lsystem:token-value i)))))

;; Single nonterminal, multiple terminals.
(lisp-unit:define-test test-tree1
  (let ((system (lsystem:create-lsystem-from-file "data/tree1.yaml")))
    (lisp-unit:assert-equal "F"
        (test-to-string (lsystem:get-lsystem-current system)))
    (setf system (lsystem:substitution system))
    (lisp-unit:assert-equal "F[+F]F[-F]F"
        (test-to-string (lsystem:get-lsystem-current system)))
    (setf system (lsystem:substitution system))
    (lisp-unit:assert-equal
        "F[+F]F[-F]F[+F[+F]F[-F]F]F[+F]F[-F]F[-F[+F]F[-F]F]F[+F]F[-F]F"
        (test-to-string (lsystem:get-lsystem-current system)))))

(lisp-unit:define-test test-tree3
  (let ((system (lsystem:create-lsystem-from-file "data/tree3.yaml")))
    (lisp-unit:assert-equal "F"
        (test-to-string (lsystem:get-lsystem-current system)))
    (setf system (lsystem:substitution system))
    (lisp-unit:assert-equal "FF+[+F-F-F]-[-F+F+F]"
        (test-to-string (lsystem:get-lsystem-current system)))
    (setf system (lsystem:substitution system))
    (lisp-unit:assert-equal
        "FF+[+F-F-F]-[-F+F+F]FF+[+F-F-F]-[-F+F+F]+[+FF+[+F-F-F]-[-F+F+F]-FF+[+F-F-F]-[-F+F+F]-FF+[+F-F-F]-[-F+F+F]]-[-FF+[+F-F-F]-[-F+F+F]+FF+[+F-F-F]-[-F+F+F]+FF+[+F-F-F]-[-F+F+F]]"
        (test-to-string (lsystem:get-lsystem-current system)))))

;; Two nonterminals, multiple terminals.
(lisp-unit:define-test test-tree2
  (let ((system (lsystem:create-lsystem-from-file "data/tree2.yaml")))
    (lisp-unit:assert-equal "X"
        (test-to-string (lsystem:get-lsystem-current system)))
    (setf system (lsystem:substitution system))
    (lisp-unit:assert-equal "F[+X][-X]FX"
        (test-to-string (lsystem:get-lsystem-current system)))
    (setf system (lsystem:substitution system))
    (lisp-unit:assert-equal "FF[+F[+X][-X]FX][-F[+X][-X]FX]FFF[+X][-X]FX"
        (test-to-string (lsystem:get-lsystem-current system)))))