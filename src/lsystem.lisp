;;;; lsystem.lisp
;;;; Implementation of the Lindenmayer System's data structure and the functions
;;;; operating on it.

(in-package #:lsystem)

(defstruct token
  (vtype :nil :type keyword)
  (value "" :type string))

(defun token-type-of-p (tok token-type)
  "Return T if token TOK is of keyword TOKEN-TYPE; else nil."
  (declare (type token tok)
           (type keyword token-type))
  (eq (token-vtype tok) token-type))

(defun string-to-int-list (str)
  "Parse the string STR returing list of ints."
  (declare (type string str)
           (optimize (speed 3) (safety 0) (debug 0)))
  (map 'list #'(lambda (char) (char-code (coerce char 'character))) str))
;;; L-System

(defclass lsystem ()
  ((rules :reader rules
          :initarg :rules
          :initform (make-hash-table)
          :type hash-table
          :documentation "Rules for the L-System.")
   (trules :reader trules
           :initarg :trules
           :initform '()
           :type cons
           :documentation "Rules for the terminals of the L-System.")
   (lookup :reader lookup
           :initarg :lookup
           :initform (make-hash-table)
           :type hash-table
           :documentation "Lookup table for tokens.")
   (history :reader history
            :initarg :history
            :initform (make-hash-table)
            :type hash-table
            :documentation "History of all iterations performed.")
   (current-iteration :reader current
                      :initarg :current
                      :initform 0
                      :type integer
                      :documentation "The current iteration of the system. Where
 current-iteration == 0 is the axiom."))
  (:documentation "Data structure for the Lindenmayer System."))

(defgeneric lsystem-count (obj count-type)
  (:documentation "Count tokens in OBJ given the keyword COUNT-TYPE; where key-
 words are :terminal, :nonterminal, or :all. Returning the integer result."))

(defgeneric lsystem-state (obj &optional iteration)
  (:documentation "Return the state of the lsystem OBJ at the specified
 ITERATION."))

(defgeneric lsystem-rule (obj rule-type rule-value)
  (:documentation "Return the rule for RULE-VALUE key of RULE-TYPE; where RULE-TYPE is a symbol
 'rules or 'trules indicating which set of rules to retrieve."))

(defgeneric token-of (obj token-key)
  (:documentation "Return the token in the lsystem OBJ that TOKEN-KEY
 identifies."))

(defgeneric substitution (obj &optional times)
  (:documentation "Perform the substitution operation on lystem OBJ the given
 number of TIMES, where the default is 1; returning the new LSYSTEM object."))

(defmethod lsystem-state ((obj lsystem) &optional
                         (iteration 0 iteration-supplied-p))
  (declare (type integer iteration)
           (type bool iteration-supplied-p))
  (if iteration-supplied-p
      (gethash iteration (history obj))
      (gethash (current obj) (history obj))))

(defmethod lsystem-count ((obj lsystem) count-type)
  (declare (type keyword count-type))
  (count-if #'(lambda (token)
                (case count-type
                  (:nonterminal
                    (token-type-of-p (token-of obj token) count-type))
                  (:terminal
                    (token-type-of-p (token-of obj token) count-type))
                  (:all t)))
            (lsystem-state obj)))

(defmethod lsystem-rule ((obj lsystem) rule-type rule-value)
  (declare (type symbol rule-type)
           (type integer rule-value))
  (let ((cvalue (string (code-char rule-value))))
    (cond ((eq rule-type 'rules)
           (gethash cvalue (rules obj)))
          ((eq rule-type 'trules)
            (car (alexandria::assoc-value (trules obj) cvalue :test 'equal)))
          (t nil))))

(defmethod token-of ((obj lsystem) token-key)
  (declare (type integer token-key))
  (gethash token-key (lookup obj)))

(defmethod substitution ((obj lsystem) &optional (times 1))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (labels ((sub-rec (lsys curr times)
            (when (<= times 0) (return-from sub-rec (history lsys)))
            (setf (gethash (1+ curr) (history lsys))
                  (alexandria:flatten
                    (map 'list #'(lambda (char)
                                    (cond ((lsystem-rule lsys 'rules char)
                                           (string-to-int-list
                                              (lsystem-rule lsys 'rules char)))
                                          (t char)))
                          (lsystem-state lsys curr))))
            (sub-rec lsys (1+ curr) (1- times))))
    (make-instance 'lsystem :rules (rules obj)
                            :trules (trules obj)
                            :lookup (lookup obj)
                            :history (sub-rec obj (current obj) times)
                            :current (+ (current obj) times))))

(defun create-lookup-table (rules-table)
  "Create a lookup table from the hash-table RULES-TABLE; returning the resulting
hash-table."
  (declare (type hash-table rules-table))
  (let ((lookup (make-hash-table :size (hash-table-size rules-table))))
    (iterate:iter
      (iterate:for rule iterate::in (alexandria:hash-table-values rules-table))
      (iterate:iter
        (iterate:for value iterate::in-vector rule)
        (unless (gethash (char-code (coerce value 'character)) lookup)
          (setf (gethash (char-code (coerce value 'character)) lookup)
                (make-token
                    :vtype (cond ((member value
                                      (alexandria:hash-table-keys rules-table)
                                      :test 'string=)
                                  :nonterminal)
                                  (t :terminal))
                    :value (string value))))))
    lookup))

(defun parse-terminal-rules (trules)
  "Rewrite terminal rules in TRULES so that 'function' strings become keywords.
 Returning the resulting alist."
  (declare (type hash-table trules))
  (iterate:iter
    (iterate:for (key value) iterate::in-hashtable trules)
    (iterate:collecting
      (list key
            (cond ((atom value)
                    (alexandria:make-keyword (string-upcase value)))
                  (t (list (alexandria:make-keyword (string-upcase (first value)))
                            (first (last value)))))))))

(defun create-lsystem-from-file (yaml-file)
  "Create the lsystem from the YAML-FILE whch is either a string or path object;
 return values of (lsystem, iterations)."
  (declare (optimize (speed 3) (safety 3) (debug 0)))

  (let* ((yaml-data (read-lsystem-file yaml-file))
         (rule-cache (with-error-validate-input yaml-data "rules"))
         (iterations (with-restart-validate-input yaml-data "iterations"))
         (init-sys (make-hash-table :size (+ iterations iterations)
                                    :rehash-threshold 0.75)))
    (setf (gethash 0 init-sys)
          (string-to-int-list (with-restart-validate-input yaml-data "axiom")))
    (values (make-instance 'lsystem
                :rules rule-cache
                :trules (parse-terminal-rules
                          (with-error-validate-input yaml-data "trules"))
                :lookup (create-lookup-table rule-cache)
                :history init-sys)
            iterations)))

(defun read-lsystem-file (path)
  "Read the string or pathname PATH and return the resulting hashtable."
  (restart-case (typecase path
                  (string (yaml:parse (pathname path)))
                  (pathname (yaml:parse path))
                  (t (error 'bad-file-input
                            :message "Cannot read specified file.")))
    (use-value (value)
      :report "Select new file."
      :interactive (lambda () (list (ask "Path: ")))
      (yaml:parse (pathname value)))))