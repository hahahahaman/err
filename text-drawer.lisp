(in-package :err)

(defstruct text-char
  (texture-id 0 :type fixnum)
  (size (ivec2 0 0) :type ivec2)
  (bearing (ivec2 0 0) :type ivec2)
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
                    (position (vec3 0.0 0.0 0.0))
                    (scale (kit.glm:vec2 1.0 1.0))
                    (color (kit.glm:vec4 1.0 1.0 1.0 1.0))
                    (rotate 0.0)
                    (drawer *text-drawer*))
  (with-slots (program vao vbo) drawer
    (gl:use-program (id program))
    (gl:uniformfv (get-uniform program "textColor") color)
    (gl:bind-vertex-array vao)
    (iter (for c in-vector text)
      (let* ((tc (@ font-text-chars c))
             (xpos (* (+ (x-val position)
                         (x-val (text-char-bearing tc)))
                      (x-val scale)))
             (ypos (* (+ (y-val position)
                         (- (y-val (text-char-bearing (@ font-text-chars #\H)))
                            (y-val (text-char-bearing tc))))
                      (y-val scale)))
             (w (* (x-val (text-char-size tc)) (x-val scale)))
             (h (* (y-val (text-char-size tc)) (y-val scale))))

        (gl:active-texture :texture0)
        (gl:bind-texture :texture-2d (text-char-texture-id tc))

        (gl:bind-buffer :array-buffer vbo)
        (gl:enable-vertex-attrib-array 0)
        (gl:vertex-attrib-pointer 0 4 :float nil (sizeof* :float 4) 0)

        (with-sequence-to-gl-array (verts
                                    (vector
                                     xpos       ypos       0.0 0.0
                                     (+ xpos w) ypos       1.0 0.0
                                     xpos       (+ ypos h) 0.0 1.0
                                     (+ xpos w) (+ ypos h) 1.0 1.0)
                                    :float)

          (gl:buffer-data :array-buffer :dynamic-draw verts))
        (gl:draw-arrays :triangle-strip 0 4)
        (incf (x-val position) (text-char-advance tc))))
    (gl:bind-buffer :array-buffer 0)
    (gl:bind-vertex-array 0)
    (gl:bind-texture :texture-2d 0)))
