;;;; lsystem.lisp
;;;; Implementation of the Lindenmayer System's data structure and the functions
;;;; operating on it.

(in-package #:lsystem)

;;; lexer

(defstruct token
  (vtype :nil :type keyword)
  (value "" :type char))

(defun tokenize (str rules-keys trules-keys)
  "Tokenize the string STR using the rules key information in lists RULES-KEYS
 and TRUES-KEYS. Returning a list of the tokenized values."
  (declare (type string str)
           (type list rules-keys trules-keys)
           (optimize (speed 3) (safety 3) (debug 0)))

  (labels ((tokenize-rec (str curr last rules-keys trules-keys)
             (when (= curr last) (return-from tokenize-rec '()))

             (let* ((value (subseq str curr (1+ curr)))
                    (t-type (cond ((member value rules-keys :test 'string=)
                                    :nonterminal)
                                  ((member value trules-keys :test 'string=)
                                    :terminal)
                                  (t :nil))))
                (append (list (make-token :vtype t-type :value value))
                        (tokenize-rec str (1+ curr) last rules-keys trules-keys)))))
    (tokenize-rec str 0 (length str) rules-keys trules-keys)))

;;; L-System

(defclass lsystem ()
  ((rules :reader rules
          :initarg :rules
          :initform (make-hash-table)
          :type hashmap
          :documentation "Rules for the L-System.")
   (trules :reader trules
           :initarg :trules
           :initform (make-hash-table)
           :documentation "Rules for the terminals of the L-System.")
   (history :reader history
            :initarg :history
            :initform (make-hash-table)
            :type hashmap
            :documentation "History of all iterations performed.")
   (current-iteration :reader current
                      :initarg :current
                      :initform 0
                      :type integer
                      :documentation "The current iteration of the system. Where
 current-iteration == 0 is the initial statement."))
  (:documentation "Data structure for the Lindenmayer System."))

(defmethod get-lsystem-current ((obj lsystem))
  "Return the current state of the lsystem OBJ."
  (gethash (current obj) (history obj)))

(defmethod get-lsystem-at ((obj lsystem) iteration)
  "Return the state of the lsytem at the specified ITERATION."
  (gethash iteration (history obj)))

(defmethod substitution ((obj lsystem))
  "Perform the substitution operation on lystem OBJ, returning the new LSYSTEM
 object."
  (declare (optimize (speed 3) (safety 0) (debug 0)))

  (let ((copy-history (alexandria:copy-hash-table (history obj))))
    (setf (gethash (1+ (current obj)) copy-history)
          (alexandria::flatten
            (iterate:iter
              (iterate:for char iterate::in (get-lsystem-current obj))
              (iterate:for char-str = (string (token-value char)))
              (iterate:collecting
                  (cond ((gethash char-str (rules obj))
                         (tokenize (gethash char-str (rules obj))
                                          (alexandria:hash-table-keys
                                              (rules obj))
                                          (alexandria:hash-table-keys
                                              (trules obj))))
                        (t char))))))
    (make-instance 'lsystem :rules (alexandria:copy-hash-table (rules obj))
                            :trules (alexandria:copy-hash-table (trules obj))
                            :history copy-history
                            :current (1+ (current obj)))))

(defmethod do-substitution-times ((obj lsystem) times)
  "Perform substitution on lsystem OBJ the give number of TIMES returning the
 last LSYSTEM."
  (declare (type integer times))
  (cond ((<= times 0) obj)
        (t (do-substitution-times (substitution obj) (1- times)))))

(defun create-lsystem-from-file (yaml-file)
  "Create the lsystem from the YAML-FILE whch is either a string or path object;
 return values of (lsystem, iterations)."
  (declare (optimize (speed 3) (safety 3) (debug 0)))

  (let* ((yaml-data (read-lsystem-file yaml-file))
        (rule-cache (with-error-validate-input yaml-data "rules"))
        (trule-cache (with-error-validate-input yaml-data "trules"))
        (init-sys (make-hash-table)))
    (setf (gethash 0 init-sys)
          (tokenize (with-restart-validate-input yaml-data "axiom")
              (alexandria:hash-table-keys rule-cache)
              (alexandria:hash-table-keys trule-cache)))
    (values (make-instance 'lsystem
                :rules (alexandria::copy-hash-table rule-cache)
                :trules (alexandria::copy-hash-table trule-cache)
                :history init-sys)
            (with-restart-validate-input yaml-data "iterations"))))

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