;;;; error.lisp
;;;; Error handling functions.
(in-package #:lsystem)

(define-condition bad-file-input (error)
  ((message :initarg :message :reader error-message))
  (:report (lambda (condition stream)
              (format stream "~S" (error-message condition))))
  (:documentation "Error condition for missing file input."))

(define-condition missing-data (error)
  ((message :initarg :message :reader error-message))
  (:report (lambda (condition stream)
              (format stream "~S" (error-message condition))))
  (:documentation "Error condition for missing data input."))

(defmacro with-restart-validate-input (input-table input-string)
  `(restart-case (multiple-value-bind (value exist-p)
                    (gethash ,input-string ,input-table)
                  (when (null exist-p)
                    (error 'missing-data :message "Data is missing."))
                  value)
    (use-value (value)
      :report (lambda (stream) (format stream "Select new ~s" ,input-string))
      :interactive (lambda () (list (ask "Value: ")))
      value)))

(defmacro with-error-validate-input (input-table input-string)
  `(multiple-value-bind (value exist-p)
        (gethash ,input-string ,input-table)
      (when (null exist-p)
        (error 'missing-data
               :message (format nil "~S data is missing." ,input-string)))
    value))

(defun ask (string)
  (princ string *query-io*)
  (read *query-io*))