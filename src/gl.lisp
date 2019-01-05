;;;; translation.lisp
;;;; Create and run the L-System through OpenGL rendering.

(in-package #:lsystem)

(define-condition bad-data (error)
  ((message :initarg :message
            :reader message)
   (value :initarg :value
          :reader value))
  (:report (lambda (condition stream)
             (format stream "~A: ~A~%" (message condition) (value condition)))))

(gl:define-gl-array-format position-color
  (gl:vertex :type :float :components (x y))
  (gl:color :type :unsigned-char :components (r g b)))

(defclass gl-object ()
  ((vertices :accessor vertices
             :initarg :vertices
             :documentation "GL array of position-color information for vertices.")
   (indices :accessor indices
            :initarg :indices
            :documentation "GL array holding the indices for the vertices."))
  (:documentation "An object renderable by OpenGL."))

(defmethod nset-gl-object-vertex ((obj gl-object) vertex x y)
  "Set X, Y values of given VERTEX in gl-object OBJ."
  (declare (type integer vertex))

  (setf (gl:glaref (vertices obj) vertex 'x) x)
  (setf (gl:glaref (vertices obj) vertex 'y) y))

(defmethod nset-gl-object-color ((obj gl-object) vertex r g b)
  "Set the RGB vlaues of the given VERTEX for the gl-object OBJ."
  (declare (type integer vertex r g b))

  (setf (gl:glaref (vertices obj) vertex 'r) r)
  (setf (gl:glaref (vertices obj) vertex 'g) g)
  (setf (gl:glaref (vertices obj) vertex 'b) b))

(defmethod nset-gl-object-index ((obj gl-object) position index)
  "Set INDEX at POSITION for gl-object OBJ."
  (declare (type integer position index))
  (setf (gl:glaref (indices obj) position) index))

;;; OpenGL-related functions.

(glfw:def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (set-viewport w h))

(glfw:def-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press)
    (glfw:set-window-should-close))))

(glfw:def-mouse-button-callback mouse-callback (window button action mod-keys)
  (declare (ignore window mod-keys action button)))

(defun create-gl-object (num-vertices num-indices)
  "Instantiate a new gl-object allocating its arrays to integers NUM-VERTICES and
 NUM-INDICES. Returning the new GL-OBJECT."
  (declare (type integer num-vertices num-indices))

  (make-instance 'gl-object
                 :vertices (gl:alloc-gl-array 'position-color num-vertices)
                 :indices (gl:alloc-gl-array :unsigned-short num-indices)))

(defun gl-cleanup (object)
  "Clean up the gl-object OBJECT."
  (gl:free-gl-array (vertices object))
  (gl:free-gl-array (indices object)))

(defun render (object)
  "Render the gl-object OBJECT onto the screen."
  (declare (type gl-object object))
  (gl:clear :color-buffer)

  (gl:enable-client-state :vertex-array)
  (gl:bind-gl-vertex-array (vertices object))
  (gl:draw-elements :lines (indices object) :offset 0))

(defun set-viewport (width height)
  "Set the viewport using the integer WIDTH and HEIGHT."
  (declare (type integer width height))

  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)

  (gl:ortho -1.0 1.0 -1.0 1.0 -1.0 1.0)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

;;; End OpenGL-related functions

(defun calculate-new-point (start-x start-y length angle)
  "Calculate the new point given START-X, START-Y, a normalized LENGTH, and an
 ANGLE (degrees). Returning multiple float values (x,y)."
  (values (coerce (+ start-x (* length (cos (degrees-to-radians angle))))
                  'short-float)
          (coerce (+ start-y  (* length (sin (degrees-to-radians angle))))
                  'short-float)))

(defun create-object (lsys translation)
  "Create a new GL-OBJECT using information from lsystem LSYS and translation
 data from hashtable TRANSLATION."
  (declare (type lsystem lsys)
           (type hashmap translation))

  (let* ((system-size (1+ (iterate:iter
                            (iterate:for ch iterate::in-vector
                                (get-lsystem-current lsys))
                            (iterate:counting (member ch (nonterminals lsys)
                                                      :test 'string=)))))
          (window (glfw:get-window-size))
          (obj (create-gl-object system-size (* system-size 2)))
          (current-angle (gethash "start-angle" translation))
          (stack '()))
    (nset-gl-object-vertex obj 0 (gethash "start-x" translation)
        (gethash "start-y" translation)) ; Origin
    (iterate:iter
      (iterate:with x1 = (gethash "start-x" translation))
      (iterate:with y1 = (gethash "start-y" translation))
      (iterate:with k = 0)
      (iterate:with k-value = 0)
      (iterate:for i iterate::in-vector (get-lsystem-current lsys))
      (iterate:for rule-translation = (gethash (string i)
                                               (gethash "rules" translation)))
      (handler-case
        (cond ((member i (nonterminals lsys) :test 'string=)
                (multiple-value-bind (x y)
                    (calculate-new-point x1 y1 (first rule-translation)
                        current-angle)
                  (nset-gl-object-vertex obj (1+ k) (/ x (first window))
                      (/ y (first (last window))))
                  (nset-gl-object-index obj (* k 2) k-value)

                  (nset-gl-object-index obj (1+ (* k 2)) k-value)
                  (cond ((= k k-value)
                          (nset-gl-object-index obj (1+ (* k 2)) (1+ k-value)))
                        (t
                          (nset-gl-object-index obj (1+ (* k 2))
                            (1+ (setf k-value k)))))
                    (setf x1 x)
                    (setf y1 y))
                  (setf k (1+ k))
                  (setf k-value (1+ k-value)))
              ((member i (terminals lsys) :test 'string=) ; TODO Clean up and make more abstract.
                (cond ((string= i #\[)
                      (push (list x1 y1 current-angle k) stack))
                    ((string= i #\])
                      (let ((values (pop stack)))
                        (setf x1 (pop values))
                        (setf y1 (pop values))
                        (setf current-angle (pop values))
                        (setf k-value (pop values))))
                    ((string= i #\+)
                     (setf current-angle (- current-angle (angle lsys))))
                    ((string= i #\-)
                     (setf current-angle (+ current-angle (angle lsys)))))))
      (error (c)
        (error 'bad-data :message "Bad translation data" :value c))))
  obj))

(defun degrees-to-radians (degrees)
  "Convert float DEGREES into radians; returning a float."
  (declare (type float degrees))
  (* degrees (/ pi 180.0)))

(defun read-translation-file (path)
  "Read the string or pathname PATH and return the resulting hashtable."
  (typecase path
    (string (yaml:parse (pathname path)))
    (pathname (yaml:parse path))
    (t (error "Unable to read file format."))))

(defun main (rules-file translation-file)
  (when (or (null rules-file) (null translation-file))
    (error "Missing data files."))

  ;; Handler-Bind is used to get rid of the constant output of warnings by
  ;; Alexandria for using deprecated bare references.
  (handler-bind
      ((alexandria:simple-style-warning
        (lambda (warning)
          (when (alexandria:starts-with-subseq
                 "bare references to struct types are deprecated."
                 (simple-condition-format-control warning))
            (muffle-warning warning)))))
    (glfw:with-init-window (:title "LSystem" :width 680 :height 400)
      (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address)

      ;; Setup callbacks
      (glfw:set-key-callback 'key-callback)
      (glfw:set-mouse-button-callback 'mouse-callback)
      (glfw:set-window-size-callback 'update-viewport)

      ;; Setup OpenGL
      (gl:clear-color 0 0 0 0)

      (multiple-value-bind (lsys iterations)
          (create-lsystem-from-file rules-file)
        (let* ((obj (create-object (do-substitution-times lsys iterations)
                                   (read-translation-file translation-file))))
          (loop until (glfw:window-should-close-p)
            do (render obj)
            do (glfw:poll-events)
            do (glfw:swap-buffers))
          (gl-cleanup obj))))))