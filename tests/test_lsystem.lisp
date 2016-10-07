(in-package #:lsystem.tests)

(lisp-unit:define-test test-alists
  (lisp-unit:assert-equal (list (cons "A" "AB") (cons "B" "A"))
                          (lsystem::create-alists (list "A=AB" "B=A"))
                          "alist incorrect"))

(lisp-unit:define-test test-parse-rules
  (let ((rules (list "A=A[B]" "B=A")))
    (lisp-unit:assert-equal (list (list "A" "B")
                                  (list #\[ #\])
                                  (list (cons "A" "A[B]")
                                        (cons "B" "A")))
                            (lsystem::parse-rules rules)
                            "rules don't match")))

(lisp-unit:define-test test-substitution
  (let* ((rules (list (list "A" "B") '() (list (cons "A" "AB")
                                              (cons "B" "A"))))
        (lsys (lsystem:create-lsystem "A" rules))
        (nlsys (lsystem::substitution lsys)))
    (lisp-unit:assert-equal (list "A" "B") (lsystem:variables nlsys)
                            "variables don't match")
    (lisp-unit:assert-equal '() (lsystem:constants nlsys)
                            "constants don't match")
    (lisp-unit:assert-equal "AB" (lsystem:lsystem nlsys)
                            "system doesn't match.")))

  
(lisp-unit:define-test test-create-lsystem
    (let* ((axiom "A")
           (rules (list (list "A" "B") (list "[" "]") (cons "A" "A=[B]")))
           (lsystem (lsystem:create-lsystem axiom rules)))
      (lisp-unit:assert-equal (list "A" "B") (lsystem:variables lsystem)
                              "variables don't match")
      (lisp-unit:assert-equal (list "[" "]") (lsystem:constants lsystem)
                              "constants don't match")
      (lisp-unit:assert-equal (cons "A" "A=[B]") (lsystem:rules lsystem)
                              "rules don't match")))
          
