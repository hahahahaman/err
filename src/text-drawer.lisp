(in-package :err)

(defstruct text-char
  (texture-id 0 :type fixnum)
  (size (vec2i 0 0) :type vec2i)
  (bearing (vec2i 0 0) :type vec2i)
  (advance 0.0 :type single-float))

(defclass text-drawer (drawer)
  ((vbo
    :accessor vbo
    :initarg :vbo))
  (:default-initargs
   :vbo (first (gl:gen-buffers 1))))

(defmethod initialize-instance :after ((drawer text-drawer) &key)
  t)

(defun text-draw (text font-text-chars
                  &key
                    (position (vec3f 0.0 0.0 0.0))
                    (scale (vec2f 1.0 1.0))
                    (color (vec4f 1.0 1.0 1.0 1.0))
                    (rotate 0.0)
                    (rotation-center (vec3f 0.0 0.0 0.0))
                    (draw-center (vec3f 0.0 0.0 0.0))
                    (draw-mode :triangles)
                    (drawer *text-drawer*))
  "=> TEXT-WIDTH, TEXT-HEIGHT
Draws a text string on screen."
  (with-slots (program vao vbo) drawer
    (gl:use-program (id program))
    (gl:uniformfv (get-uniform program "textColor") color)
    (gl:bind-vertex-array vao)
    (let ((total-width 0.0)
          (lowest-y 0.0)
          (highest-y 0.0))
      ;; iteratively render all characters of TEXT
      (iter (for c in-vector text)
        (let* ((tc (@ font-text-chars c)) ;; text-char
               (text-char-bearing (text-char-bearing tc))
               (text-char-size (text-char-size tc))
               (text-char-advance (text-char-advance tc))
               (tc-bearing (vec2f-mul (vec2f (cfloat (x-val text-char-bearing))
                                             (cfloat (y-val text-char-bearing)))
                                      scale))
               (tc-size (vec2f-mul (vec2f (cfloat (x-val text-char-size))
                                          (cfloat (y-val text-char-size)))
                                   scale))
               (tc-advance (* (x-val scale) text-char-advance))
               (xpos (+ (x-val position)
                        (x-val tc-bearing)))
               (ypos (+ (y-val position)
                        (y-val tc-bearing)))
               (w (x-val tc-size))
               (h (y-val tc-size)))

          ;; find height of the rendered text
          ;; check if new lowest y position
          (when (< ypos lowest-y)
            (setf lowest-y ypos))
          ;; check if new highest y position
          (when (> (+ ypos h) highest-y)
            (setf highest-y (+ ypos h)))

          ;; gl rendering stuff
          (gl:active-texture :texture0)
          (gl:bind-texture :texture-2d (text-char-texture-id tc))

          (gl:bind-buffer :array-buffer vbo)
          (gl:enable-vertex-attrib-array 0)
          (gl:vertex-attrib-pointer 0 4 :float nil (sizeof* :float 4) 0)

          (with-sequence-to-gl-array (verts
                                      (vector
                                       xpos       ypos       0.0 0.0
                                       (+ xpos w) ypos       1.0 0.0
                                       xpos       (- ypos h) 0.0 1.0
                                       (+ xpos w) (- ypos h) 1.0 1.0)
                                      :float)

            (gl:buffer-data :array-buffer :dynamic-draw verts))
          (gl:draw-arrays :triangle-strip 0 4)

          (incf (x-val position) tc-advance)
          (incf total-width tc-advance)))

      ;; unbind all gl stuff
      (gl:bind-buffer :array-buffer 0)
      (gl:bind-vertex-array 0)
      (gl:bind-texture :texture-2d 0)

      ;; width and height returned
      (values total-width
              (- highest-y lowest-y)))))

(defun text-width (text font-text-chars
                   &key
                     (scale (vec2f 1.0 1.0)))
  "=> WIDTH
Finds the total width of a string of text rendered with given font characters
and scale."
  (let ((width 0.0))
    (iter (for c in-vector text)
      (let ((tc (@ font-text-chars c)))
        (incf width (text-char-advance tc))))
    width))

(defun text-height (text font-text-chars
                    &key
                      (scale (vec2f 1.0 1.0)))
  "=> HEIGHT
Finds the total height of a string of text rendered with the given font
characters and scale."
  (let ((lowest-y 0.0)
        (highest-y 0.0))
    (iter (for c in-vector text)
      (let* ((tc (@ font-text-chars c))
             (ypos (* (y-val (text-char-bearing tc))
                      (y-val scale)))
             (h (* (y-val (text-char-size tc))
                   (y-val scale))))
        (when (< ypos lowest-y)
          (setf lowest-y ypos))
        (when (> (+ ypos h) highest-y)
          (setf highest-y (+ ypos h)))))
    (- highest-y lowest-y)))

(defun text-dimensions (text font-text-chars
                        &key
                          (scale (vec2f 1.0 1.0)))
  "=> WIDTH, HEIGHT
Returns the width and height of the the text string with the given font
characters and scale."
  (values (text-width text font-text-chars :scale scale)
          (text-height text font-text-chars :scale scale)))
