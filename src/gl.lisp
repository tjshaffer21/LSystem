;;;; translation.lisp
;;;; Create and run the L-System through OpenGL rendering.

(in-package #:lsystem)

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

(defun nset-new-gl-point (gl-obj xy len angle k)
  "Calculate and set new values for GL-OBJ array using the given a list of
 floats XY, floats LEN and ANGLE, and index list K (k, k-prev). The GL-OBJ is
 modified, and new values for XY and K are returned."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((k (first k))
        (k-prev (first (last k))))
    (multiple-value-bind (nx ny)
        (calculate-new-point (first xy) (first (last xy)) len angle)
      (nset-gl-object-vertex gl-obj (1+ k) nx ny)
      (nset-gl-object-index gl-obj (+ k k) k-prev)

      (nset-gl-object-index gl-obj (1+ (+ k k)) k-prev)
      (nset-gl-object-index gl-obj (1+ (+ k k))
                                    (if (= k k-prev)
                                        (1+ k-prev)
                                        (1+ (setf k-prev k))))
      (values (list nx ny) (list (1+ k) (1+ k-prev))))))

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

  (iterate:iter
    (iterate:with system-size = (1+ (lsystem-count lsys :nonterminal)))
    (iterate:with obj = (create-gl-object system-size
                                          (+ system-size system-size)))
    (iterate:with stack = '())
    (iterate:with current-angle =
        (with-restart-validate-input translation "start-angle"))
    (iterate:with xy =
        (list (with-restart-validate-input translation "start-x")
              (with-restart-validate-input translation "start-y")))
    (iterate:with k = (list 0 0))
    (iterate:with translation-rules =
        (with-error-validate-input translation "rules")) ; Cache
    (iterate:for i iterate::in (lsystem-state lsys))
    (iterate:for rule-translation =
        (gethash (token-value (token-of lsys i)) translation-rules))
    (iterate:if-first-time
        (nset-gl-object-vertex obj 0 (first xy) (first (last xy))))
    (case (token-vtype (token-of lsys i))
          (:nonterminal
            (multiple-value-bind (nxy nk)
                (nset-new-gl-point obj xy (first rule-translation)
                                  current-angle k)
              (setf xy nxy)
              (setf k nk)))
          (:terminal
              (let ((cache-rule (lsystem-rule lsys 'trules i)))
                (cond ((listp cache-rule)
                       (when (eq (first cache-rule) :angle)
                          (setf current-angle (+ current-angle
                                                (first (last cache-rule))))))
                      ((eq cache-rule :push)
                       (push (list xy current-angle (first k)) stack))
                      ((eq cache-rule :pop)
                       (let ((values (pop stack)))
                          (setf xy (pop values))
                          (setf current-angle (pop values))
                          (setf k (list (first k) (pop values)))))
                      (t (warn "Unimplemented terminal rule detected!")))))
          (t (warn "Unimplemented rule detected")))
    (iterate:finally (iterate::return obj))))

(defun degrees-to-radians (degrees)
  "Convert float DEGREES into radians; returning a float."
  (declare (type float degrees))
  (* degrees 0.017453292519943295D0))

(defun read-translation-file (path)
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

(defun main (rules-file translation-file)
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
        (let ((obj (create-object (substitution lsys iterations)
                                  (read-translation-file translation-file))))
          (loop until (glfw:window-should-close-p)
            do (render obj)
            do (glfw:poll-events)
            do (glfw:swap-buffers))
          (gl-cleanup obj))))))