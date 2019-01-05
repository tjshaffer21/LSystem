;;;; lsystem.lisp
;;;; Implementation of the Lindenmayer System's data structure and the functions
;;;; operating on it.

(in-package #:lsystem)

(defclass lsystem ()
  ((rules :reader rules
          :initarg :rules
          :initform (make-hash-table)
          :type hashmap
          :documentation "Rules for the L-System.")
   (terminals :reader terminals
              :initarg :terminals
              :initform '()
              :type list
              :documentation "Set of terminals in the system.")
   (nonterminals :reader nonterminals
                 :initarg :nonterminals
                 :initform '()
                 :type list
                 :documentation "Set of nonterminals in the system.")
   (history :reader history
            :initarg :history
            :initform (make-hash-table)
            :type hashmap
            :documentation "Cache of iterations.")
   (current-iteration :reader current
                      :initarg :current
                      :initform 0
                      :type integer
                      :documentation "The current iteration of the system. Where
 current-iteration == 0 is the initial statement.")
   (angle :reader angle
          :initarg :angle
          :initform 90.0
          :type float
          :documentation ""))
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
  (let ((copy-rules (alexandria:copy-hash-table (rules obj)))
        (copy-history (alexandria:copy-hash-table (history obj))))
    (setf (gethash (1+ (current obj)) copy-history)
          (apply #'concatenate 'string
            (iterate:iter
              (iterate:for char iterate::in-vector
                  (gethash (current obj) (history obj)))
              (iterate:for res = (gethash (string char) (rules obj)))
              (if res
                  (iterate::collect res)
                  (iterate::collect (string char))))))
    (make-instance 'lsystem :rules copy-rules
                            :terminals (copy-list (terminals obj))
                            :nonterminals (copy-list (nonterminals obj))
                            :history copy-history
                            :current (1+ (current obj))
                            :angle (angle obj))))

(defmethod do-substitution-times ((obj lsystem) times)
  "Perform substitution on lsystem OBJ the give number of TIMES returning the
 last LSYSTEM."
  (declare (type integer times))
  (cond ((<= times 0) obj)
        (t (do-substitution-times (substitution obj) (1- times)))))

(defun create-lsystem-from-file (yaml-file)
  "Create the lsystem from the YAML-FILE whch is either a string or path object;
 return values of (lsystem, iterations)."
  (let ((yaml-data (read-lsystem-file yaml-file))
        (init-sys (make-hash-table)))
    (setf (gethash 0 init-sys) (gethash "initiator" yaml-data))
    (values (make-instance 'lsystem
                  :rules (alexandria::copy-hash-table (gethash "rules" yaml-data))
                  :terminals (copy-list (gethash "terminals" yaml-data))
                  :nonterminals (copy-list (gethash "nonterminals" yaml-data))
                  :history init-sys
                  :angle (gethash "angle" yaml-data))
            (gethash "iterations" yaml-data))))

;;; TODO Error checking.
(defun read-lsystem-file (path)
  "Read the string or pathname PATH and return the resulting hashtable."
  (typecase path
      (string (yaml:parse (pathname path)))
      (pathname (yaml:parse path))
      (t (error "Unable to read file format."))))