(in-package :err)

(defstruct text-char
  (texture-id 0 :type fixnum)
  (size (ivec2 0 0) :type ivec2)
  (bearing (ivec2 0 0) :type ivec2)
  (advance 0 :type fixnum))

(defclass text-drawer (drawer)
  (text-chars
   :accessor text-chars
   :initarg :text-chars)
  (vbo
   :initarg :vbo)
  (:default-initargs
   :text-chars (empty-map)
   :vbo (first (gl:gen-buffers 1))))

(defmethod initialize-instance :after ((drawer text-drawer) &key)
  ;; Configure VAO/VBO for texture quads
  (with-slots (vbo vao) drawer
    (gl:bind-vertex-array drawer)
    (gl:bind-buffer :array-buffer vbo)
    (gl:buffer-data :array-buffer :dynamic-draw nil 0 (size-of* :float (* 6 4)))
    (gl:enable-vertex-attrib-array 0)
    (gl:vertex-attrib-pointer 0 4 :float nil (size-of* :float 4) 0)
    (gl:bind-buffer :array-buffer 0)
    (gl:bind-vertex-array 0)))


(defmethod text-draw ((text string)
                      (position vector)
                      &optional
                        (scale (kit.glm:vec2 10.0 10.0))
                        (color (kit.glm:vec4 1.0 1.0 1.0 1.0))
                        (rotate 0.0)
                        (drawer *text-drawer*))
  (with-slots (program vao text-chars) drawer
    (gl:use-program (id drawer))
    (gl:uniformfv (get-uniform program "textColor") color)
    (gl:active-texture :texture0)
    (gl:bind-vertex-array vao)
    (do-map (ch char-texture text-chars)
      (let ((pos (vec2 (* (+ (x-val position) (x-val (text-char-bearing ch)))
                          (x-val scale))
                       (* (+ y
                             (- (y-val (text-char-bearing (@ text-chars #\H)))))
                          (y-val scale))))
            (dim (vec2 (* (x-val (ch))))))))))
