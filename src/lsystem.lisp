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
   (trules :reader trules
           :initarg :trules
           :initform (make-hash-table)
           :documentation "Rules for the terminals of the L-System.")
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
  (let ((copy-history (alexandria:copy-hash-table (history obj))))
    (setf (gethash (1+ (current obj)) copy-history)
          (format nil "~{~A~}" (iterate:iter
                                  (iterate:for char iterate::in-vector
                                      (get-lsystem-current obj))
                                  (iterate:for res =
                                      (gethash (string char) (rules obj)))
                                  (if res
                                      (iterate::collect res)
                                      (iterate::collect (string char))))))
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
  (let ((yaml-data (read-lsystem-file yaml-file))
        (init-sys (make-hash-table)))
    (setf (gethash 0 init-sys)
          (with-restart-validate-input yaml-data "axiom"))
    (values (make-instance 'lsystem
                :rules (alexandria::copy-hash-table
                          (with-error-validate-input yaml-data "rules"))
                :trules (alexandria::copy-hash-table
                          (with-error-validate-input yaml-data "trules"))
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