;;;; lsystem.lisp
;;;; Implementation of the Lindenmayer System's data structure and the functions
;;;; operating on it.

(in-package #:lsystem)

(define-condition malformed-grammar (warning)
  ((message :initarg :message
            :reader message)
   (value :initarg :value
          :reader value))
  (:report (lambda (condition stream)
             (format stream "~a~a~%" (message condition) (value condition)))))

(defclass lsystem ()
  ((system :reader lsystem
           :initarg :lsystem
           :documentation "The system representation's current form.")
   (variables :reader variables
              :initarg :variables
              :type list
              :documentation "A list of all the variables.")
   (constants :reader constants
              :initarg :constants
              :type list
              :documentation "A list of all the constants.")
   (rules :reader rules
          :initarg :rules
          :initform '()
          :type list
          :documentation "AList of the rules for the system.")
   (len :reader len
        :initarg :length
        :documentation "The size of the lsystem both variables and constants."))
  (:documentation "Structure for a Lindenmayer System implementation."))

(defmethod substitution ((lsystem lsystem))
  "Apply substitution rules onto the LSYSTEM.
 Args
   lsystem - Instance of the lsystem class.
 Return
   A new instance of the lsystem."
  (let ((sys (apply #'concatenate 'string
                    (iterate:iter
                     (iterate:for v iterate::in-vector (lsystem lsystem))
                     (let ((res (assoc v (rules lsystem) :test 'string=)))
                       (cond ((null res) (iterate:collect (string v)))
                             (t (iterate:collect (cdr res)))))))))
    (make-instance
     'lsystem
     :lsystem sys
     :length (length sys)
     :variables (variables lsystem)
     :constants (constants lsystem)
     :rules (rules lsystem))))

(defmethod substitute-for ((lsystem lsystem) iterations)
  "Perform substitution on LSYSTEM for ITERATIONS
 Args
   lsystem - An instance of the lsystem class.
   iteration - interger value for iterations to perform.
 Return
   If iterations <= 0 then the original lsystem; else a new instance."
  (declare (type integer iterations))

  (cond ((> iterations 0)
         (substitute-for (substitution lsystem) (1- iterations)))
        (t lsystem)))

(defmethod print-object ((obj lsystem) out)
  (print-unreadable-object (obj out :type t)
    (format out "Current: ~s Length: ~s Variables: ~s Constants: ~s Rules: ~s"
            (lsystem obj) (len obj) (variables obj) (constants obj)
            (rules obj))))

(defun create-lsystem (axiom rules)
  "Create an instance of the lsystem class.
 Args
   AXIOM is the initial start point of the system
   RULES is a list of variables, constants, and rules (alist). See parse-rules.
 Return
   A new instance of lsystem."
  (declare (type string axiom) (type list rules))

  (make-instance 'lsystem
                 :lsystem axiom
                 :length (length axiom)
                 :variables (first rules)
                 :constants (second rules)
                 :rules (first (last rules))))

(defun create-alists (rules)
  "Parse list of RULES into an alist.
 Args
   rules - a list of strings.
 Return
   A list of alists with both elements being strings."
  (let ((rule (first rules))
        (parsed '()))
    (cond ((typep rule 'string)
           (cond ((char= (elt rule 1) #\=)
                  (push (cons (subseq rule 0 1) (subseq rule 2)) parsed))
                 (t (malformed-grammar-warning rule))))
          (t (malformed-grammar-warning rule)))
    (when (not (null (rest rules)))
      (setf parsed (append parsed (create-alists (rest rules)))))
    parsed))

(defun parse-rules (rules)
  "Parse a list of RULES strings
 Args
   rules - a list of strings.
 Return
   A list of variables, constants, and rules (alist)."
  (let* ((arules (create-alists rules))
         (vars (map 'list #'first arules))
         (const (alexandria:flatten
                 (iterate:iter
                   (iterate:for i iterate:in arules)
                   (iterate:collect (iterate:iter
                                      (iterate:for j iterate::in-sequence
                                                   (cdr i))
                                      (unless (member j vars :test 'string=)
                                        (iterate:collect j))))))))
    (list vars const arules)))

(defun read-lsystem-file (file)
  "Parse the FILE.
 See README for proper file layout.
 Args
   file - A string holding the file location.
 Return
   A list of [axiom iterations [rules]]"
  (declare (type string file))

  (with-open-file (stream file :direction :input)
    (when stream
      (let ((results '())
            (rules '()))

        ;; Rules
        (iterate:iter
         (iterate:with line = (read-line stream nil))
         (iterate:while (and line (not (string= line #\return))))
         (push (string-trim '(#\return) line) rules)
         (setf line (read-line stream nil)))
        (push (reverse rules) results)

        ;; Number of iterations
        (push (parse-integer (read-line stream nil)) results)

        ;; Axiom
        (push (string-trim '(#\return) (read-line stream nil)) results)
        results))))

(defun malformed-grammar-warning (value)
  (warn 'malformed-grammar :message "Malfrmed grammar: " :value value))
