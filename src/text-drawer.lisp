(in-package :err)

(defstruct text-char
  (texture-id 0 :type fixnum)
  (size (vec2i 0 0) :type vec2i)
  (bearing (vec2i 0 0) :type vec2i)
  (advance 0.0 :type single-float))

(defclass text-drawer (drawer)
  ((vbo
    :accessor vbo
    :initarg :vbo)
   (data-array
    :type gl:gl-array))
  (:default-initargs
   :vbo (gl:gen-buffer)))

(defmethod initialize-instance :after ((drawer text-drawer) &key)
  (with-slots (vbo data-array) drawer
    (setf data-array (gl:alloc-gl-array :float 16))
    (trivial-garbage:finalize drawer (lambda ()
                                       (gl:free-gl-array data-array)
                                       (gl:delete-buffers (vector vbo))))))

;; TODO get rotation to work
(defun text-draw (text font-text-chars
                  &key
                    (position (vec3f 0.0 0.0 0.0))
                    (scale (vec2f 1.0 1.0))
                    (color (vec4f 1.0 1.0 1.0 1.0))
                    (rotation 0.0)
                    (rotation-center (vec3f 0.0 0.0 0.0))
                    (draw-center (vec3f 0.0 0.0 0.0))
                    (drawer *text-drawer*))
  "=> TEXT-WIDTH, TEXT-HEIGHT
Draws a text string on screen."
  (with-slots (program vao vbo data-array) drawer
    (gl:use-program (id program))
    (gl:uniformfv (get-uniform program "textColor") color)
    (gl:bind-vertex-array vao)
    ;; iteratively render all characters of TEXT
    (multiple-value-bind (width height) (text-dimensions
                                         text
                                         font-text-chars
                                         :scale scale)
      (let ((model (kit.glm:matrix*
                    ;; order of operations is bottom to top since
                    ;; matrix multiplication

                    ;; ;;finally move to POSITION
                    ;; (kit.glm:translate position)

                    ;; ;; move to draw center
                    ;; (kit.glm:translate* (cfloat (* (x-val draw-center)
                    ;;                                width))
                    ;;                     (cfloat (* (y-val draw-center)
                    ;;                                height))
                    ;;                     (cfloat (* (z-val draw-center)
                    ;;                                1.0)))

                    ;; move back from rotation center
                    (kit.glm:translate* (cfloat (* (x-val rotation-center)
                                                   width))
                                        (cfloat (* (y-val rotation-center)
                                                   height))
                                        (cfloat (* (z-val rotation-center)
                                                   1.0)))

                    ;; ;; rotate around the z-axis
                    (kit.glm:rotate* 0.0 0.0 (cfloat rotation))

                    ;; move to rotation center
                    (kit.glm:translate* (cfloat (* -1.0
                                                   (x-val rotation-center)
                                                   width))
                                        (cfloat (* -1.0
                                                   (y-val rotation-center)
                                                   height))
                                        (cfloat (* -1.0
                                                   (z-val rotation-center)
                                                   1.0)))

                    ;; scale first, z axis remain constant since 2d
                    ;; (kit.glm:scale* (cfloat (x-val scale)) (cfloat (y-val scale)) 0.0)
                    )))

        ;; set model uniform
        (gl:uniform-matrix-4fv (get-uniform program "model") (vector model) nil))
      (let ((xpos (- (x-val position) (* width 0.5) (* width (x-val draw-center))))
            (ypos (- (y-val position) (* height 0.5) (* height (y-val draw-center)))))
        (iter (for c in-vector text)
          (let* ((tc (@ font-text-chars c)) ;; text-char
                 ;; values from TC
                 (text-char-bearing (text-char-bearing tc))
                 (text-char-size (text-char-size tc))
                 (text-char-advance (text-char-advance tc))
                 ;; scaled values
                 (tc-bearing (vec2f-mul (vec2f (cfloat (x-val text-char-bearing))
                                               (cfloat (y-val text-char-bearing)))
                                        scale))
                 (tc-size (vec2f-mul (vec2f (cfloat (x-val text-char-size))
                                            (cfloat (y-val text-char-size)))
                                     scale))
                 (tc-advance (* (x-val scale) text-char-advance))
                 (x (+ xpos (x-val tc-bearing)))
                 (y (+ ypos (y-val tc-bearing)))
                 (w (x-val tc-size))
                 (h (y-val tc-size)))

            ;; gl rendering stuff

            ;; OPTIMIZATION?
            ;; bind TEXTURE2D unless it is already bound
            ;; (unless (eql (id texture2d) current-texture-id)
            ;;   (gl:active-texture :texture0)
            ;;   (gl:bind-texture :texture-2d (id texture2d))
            ;;   (setf current-texture-id (id texture2d)))

            (gl:active-texture :texture0)
            (gl:bind-texture :texture-2d (text-char-texture-id tc))

            (gl:bind-buffer :array-buffer vbo)
            (gl:enable-vertex-attrib-array 0)
            (gl:vertex-attrib-pointer 0 4 :float nil (sizeof* :float 4) 0)

            (flet ((set-data (array &rest data)
                     (labels ((iter-rest (l n)
                                (when (and (< n 16) l)
                                  (setf (gl:glaref array n) (car l))
                                  (iter-rest (cdr l) (1+ n)))))
                       (iter-rest data 0))))
              (set-data data-array
                        x       y       0.0 0.0
                        (+ x w) y       1.0 0.0
                        x       (- y h) 0.0 1.0
                        (+ x w) (- y h) 1.0 1.0))
            (gl:buffer-data :array-buffer :dynamic-draw data-array)

            ;; (with-sequence-to-gl-array (verts
            ;;                             (vector x       y       0.0 0.0
            ;;                                     (+ x w) y       1.0 0.0
            ;;                                     x       (- y h) 0.0 1.0
            ;;                                     (+ x w) (- y h) 1.0 1.0)
            ;;                             :float)
            ;;   (gl:buffer-data :array-buffer :dynamic-draw verts))
            (gl:draw-arrays :triangle-strip 0 4)

            (incf xpos tc-advance))))
      (values width height))))

(defun text-width (text font-text-chars
                   &key
                     (scale (vec2f 1.0 1.0)))
  "=> WIDTH
Finds the total width of a string of text rendered with given font characters
and scale."
  (let ((width 0.0))
    (iter (for c in-vector text)
      (let ((tc (@ font-text-chars c)))
        (incf width (* (text-char-advance tc) (x-val scale)))))
    width))

(defun text-height (text font-text-chars
                    &key
                      (scale (vec2f 1.0 1.0)))
  "=> HEIGHT
Finds the total height of a string of text rendered with the given font
characters and scale."
  (let ((highest-y 0.0))
    (iter (for c in-vector text)
      (let* ((tc (@ font-text-chars c))
             (h (* (y-val (text-char-size tc))
                   (y-val scale))))
        (when (> h highest-y)
          (setf highest-y h))))
    highest-y))

(defun text-dimensions (text font-text-chars
                        &key
                          (scale (vec2f 1.0 1.0)))
  "=> WIDTH, HEIGHT
Returns the width and height of the the text string with the given font
characters and scale."
  (let ((width 0.0)
        ;; (lowest-y 0.0)
        (highest-y 0.0)
        )
    (iter (for c in-vector text)
      (let* ((tc (@ font-text-chars c))
             (h (* (y-val (text-char-size tc))
                   (y-val scale))))
        (when (> h highest-y)
          (setf highest-y h))
        (incf width (* (text-char-advance tc) (x-val scale)))))
    (values width highest-y)))
