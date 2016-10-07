;;;; main.lisp
;;;; Simple execution script to load LSystem an execute its main function.
;;;; Not to be confused with src/main which is the top-level code for the
;;;; package, and *should* ideally be renamed to an appropriate name later.

(ql:quickload :cl-utilities)
(ql:quickload :iterate)
(ql:quickload :lsystem)

(let ((cmds (or
             #+CCL *unprocessed-command-line-arguments*
             nil))
      (rules nil)
      (translation nil))
  (iterate:iter
   (iterate:with temp = nil)
   (iterate:for i iterate:in cmds)

   (setf temp (cl-utilities:split-sequence #\= i))
   (cond ((string= (first temp) "-rules")
          (setf rules (first (last temp))))
         ((string= (first temp) "-translate")
          (setf translation (first (last temp))))))    
  (lsystem:main rules translation))

(quit)
