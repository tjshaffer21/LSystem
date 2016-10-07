;;;; translation.lisp
;;;; Given the proper input, translation functions create the lsystem and then
;;;; converts the results into the desired structure.
;;;;
;;;; Available Structures
;;;;   opengl (rendered, not exported)

(in-package #:lsystem)

(gl:define-gl-array-format position-color
  (gl:vertex :type :float :components (x y))
  (gl:color :type :unsigned-char :components (r g b)))

(defclass gl-object ()
  ((vertices :accessor vertices
             :initarg :vertices
             :documentation "GL array holding position-color information for /
vertices.")
   (indices :accessor indices
            :initarg :indices
            :documentation "GL array holding the indices for the vertices."))
  (:documentation "An object renderable by OpenGL."))

(defmethod nset-gl-object-vertex ((obj gl-object) vertex x y)
  "Set the XY values of the given VERTEX for the given OBJ.
 X Y values are assumed to have been normalized.
 Args
   OBJ    - Instance of gl-object
   VERTEX - Integer for the vertex position (zero-based)
   X      - x value
   Y      - y value"
  (declare (type integer vertex))

  (setf (gl:glaref (vertices obj) vertex 'x) x)
  (setf (gl:glaref (vertices obj) vertex 'y) y))

(defmethod nset-gl-object-color ((obj gl-object) vertex r g b)
  "Set the RGB vlaues of the given VERTEX for the given OBJ.
 Args
   OBJ    - Instance of gl-object
   VERTEX - Integer for the vertex position (zero-based)
   R      - Red value
   G      - Green value
   B      - Blue value"
  (declare (type integer vertex r g b))

  (setf (gl:glaref (vertices obj) vertex 'r) r)
  (setf (gl:glaref (vertices obj) vertex 'g) g)
  (setf (gl:glaref (vertices obj) vertex 'b) b))

(defmethod nset-gl-object-index ((obj gl-object) position index)
  "Set the INDEX at the given POSITION for the OBJ. Where, INDEX is the index in
 the vertices array.
 Args
   OBJ      - Instance of gl-object
   POSITION - Integer for the index position (zero-based)
   INDEX    - Integer for the position in the vertex array (zero-based)"
  (declare (type integer position index))
  
  (setf (gl:glaref (indices obj) position) index))

;;; OpenGL-related functions.

(glfw:def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (set-viewport w h))

(glfw:def-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (glfw:set-window-should-close)))

(glfw:def-mouse-button-callback mouse-callback (window button action mod-keys)
  (declare (ignore window mod-keys action button)))

(defun set-viewport (width height)
  "Set the viewport with the new WIDTH HEIGHT values.
 Args
   WIDTH  - integer
   HEIGHT - integer"
  (declare (type integer width height))

  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  
  (gl:ortho -1.0 1.0 -1.0 1.0 -1.0 1.0)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defun render (object)
  "Render onto the screen.
 Args
   OBJECT - Instance of gl-object"
  (declare (type gl-object object))
  (gl:clear :color-buffer)

  (gl:enable-client-state :vertex-array)
  (gl:bind-gl-vertex-array (vertices object))
  (gl:draw-elements :lines (indices object)))

;;; End OpenGL-related functions

(defun calculate-new-point (start-x start-y length angle)
  "Calculate a new point.
 Args:
   START-X - x-value
   START-Y - y-value
   LENGTH  - the distance of the line (b), and is assumed normalized.
   ANGLE   - the slope and assumed to be in degrees.
 Return
   Multiple values x,y as a short-float."
  (values (coerce (+ start-x (* length (cos (degrees-to-radians angle))))
                  'short-float)
          (coerce (+ start-y (* length (sin (degrees-to-radians angle))))
                  'short-float)))

(defun create-gl-object (num-vertices num-indices)
  "Instantiate a new gl-object with NUM-VERTICES and NUM-INDICES allocated.
 Args
   NUM-VERTICES - Integer of the number of vertices for the gl-object.
   NUM-INDICES  - Integer of the number of indices for the gl-object.
 Return
   A new instance for gl-object."
  (declare (type integer num-vertices num-indices))

  (make-instance 'gl-object
                 :vertices (gl:alloc-gl-array 'position-color num-vertices)
                 :indices (gl:alloc-gl-array :unsigned-short num-indices)))

(defun create-object (lsys translation start-x start-y current-angle)
  "Instantiate a gl-object class using the information in the LSystem and
 the TRANSLATION file.
 Args
   LSYS - An instance of lsystem
   START-X - Float; assumed normalized; the starting x-value for the object.
   START-Y - Float; assumed normalized; the starting y-value for the object.
   CURRENT-ANGLE - Float; The starting angle for the object.
 Return
   An instance of gl-object"
  (declare (type lsystem lsys) (type array translation))

  (let* ((system-size (iterate:iter
                        (iterate:for i iterate::in-vector (lsystem lsys))
                        (iterate:counting (member i (variables lsys)
                                                  :test 'string=))))
         (window (glfw:get-window-size))
         (object (create-gl-object system-size (* system-size 2))))
    (nset-gl-object-vertex object 0 start-x start-y)
    (iterate:iter
     (iterate:with x1 = start-x)
     (iterate:with y1 = start-y)
     (iterate:with c = 0)
     (iterate:for i iterate::in-vector (lsystem lsys))

     (let ((trans (first (iterate:iter
                    (iterate:for j iterate::in-vector translation)
                    (when (string= (elt j 0) i)
                      (iterate:collecting j)
                      (iterate:finish))))))
       (unless (null trans) ;; TODO Should never be null when fully implemented.
         (multiple-value-bind (x y)
             (calculate-new-point x1 y1 (parse-integer (elt trans 1))
                                  current-angle)

           ;; TODO: There's probably a better way to handle this.
           (setf x (/ x (first window)))
           (setf y (/ y (first (last window))))
           
           (nset-gl-object-vertex object (1+ c) x y)
           (nset-gl-object-index object (* 2 c) c)
           (nset-gl-object-index object (1+ (* c 2)) (mod (+ c 2) system-size))
         
           (setf x1 x)
           (setf y1 y)
           (setf c (1+ c))))))
    object))

(defun degrees-to-radians (degrees)
  "Convience function to convert DEGREES to radians.
 Args
   DEGREES - Float
 Return
   A float"
  (* degrees (/ pi 180.0)))

(defun read-translation-file (file)
  "Read and parse the translation FILE (string).
 Args
   FILE - String value for the file location.
 Return
   A vector of vectors containing the translation data."
  (declare (type string file))
  
  (with-open-file (stream file :direction :input)
    (when stream
      (let ((results '()))
        (read-line stream nil) ; TODO Later implementation.

        (iterate:iter
         (iterate:with line = nil)
         (iterate:while (setf line (read-line stream nil)))
         (let ((rules (cl-utilities:split-sequence #\: (string-trim '(#\return)
                                                                    line))))
           (push (map 'array #'identity rules) results)))
        (map 'array #'identity (reverse results))))))

(defun gl-cleanup (object)
  "Free the array data from gl-object.
 Args
   OBJECT - gl-object"
  (gl:free-gl-array (vertices object))
  (gl:free-gl-array (indices object)))

(defun main (rules-file translation-file)
  ;; TODO Better error handling.
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
      
      (let* ((rules (read-lsystem-file rules-file))
             (translation (read-translation-file translation-file))
             (lsys (substitute-for
                    (create-lsystem (first rules)
                                    (parse-rules (first (last rules))))
                    (second rules)))
             (object (create-object lsys translation 0.0 0.0 90.0)))     
        ;; Event loop
        (loop until (glfw:window-should-close-p)
           do (render object)
           do (glfw:poll-events)
           do (glfw:swap-buffers))
        (gl-cleanup object)))))