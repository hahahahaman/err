(in-package :err)

(defclass rect-drawer (drawer)
  ())

(defmethod initialize-instance :after ((drawer rect-drawer) &key)
  (with-slots (vao) drawer
    (let* ((buffers (gl:gen-buffers 1))
           (vbo (first buffers)))
      (gl:bind-vertex-array vao)
      (gl:bind-buffer :array-buffer vbo)
      (with-sequence-to-gl-array (verts
                                  (vector 0.0 0.0
                                          1.0 0.0
                                          0.0 1.0
                                          1.0 1.0)
                                  :float)
        (gl:buffer-data :array-buffer :static-draw verts))

      ;; (gl:bind-buffer :element-array-buffer ebo)
      ;; (with-sequence-to-gl-array (verts
      ;;                             (vector 0 1 3 2 1)
      ;;                             :unsigned-int)
      ;;   (gl:buffer-data :element-array-buffer :static-draw verts))

      (gl:enable-vertex-attrib-array 0)
      (gl:vertex-attrib-pointer 0 2 :float nil (sizeof* :float 2) 0)

      (gl:bind-buffer :array-buffer 0)
      ;; (gl:bind-buffer :element-array-buffer 0)
      (gl:bind-vertex-array 0)

      (gl:delete-buffers buffers))))

(defun rect-draw (&key
                    (position (vec3 0.0 0.0 0.0))
                    (size (vec2 10.0 10.0))
                    (color (vec4 1.0 1.0 1.0 1.0))
                    (rotate 0.0)
                    (draw-mode :triangle-strip)
                    (drawer *rect-drawer*))
  (with-accessors ((program program) (vao vao)) drawer
    (use program)

    (gl:uniformfv (get-uniform program "rectColor") color)
    (let ((model (kit.glm:matrix*
                  ;;finally move to POS
                  (kit.glm:translate (kit.glm:vec3 (x-val position)
                                                   (y-val position)
                                                   (z-val position)))
                  ;; move top left to 0.0, 0.0
                  (kit.glm:translate* (cfloat (* 0.5 (x-val size)))
                                      (cfloat (* 0.5 (y-val size)))
                                      0.0)
                  ;; rotate around the z-axis
                  (kit.glm:rotate* 0.0 0.0 (cfloat rotate))
                  ;; move center to 0.0, 0.0, 0.0
                  (kit.glm:translate* (cfloat (* -0.5 (x-val size)))
                                      (cfloat (* -0.5 (y-val size)))
                                      0.0)
                  ;; scale first, z axis remain constant since 2d
                  (kit.glm:scale (kit.glm:vec3 (x-val size) (y-val size) 0.0)))))

      ;; set model uniform
      (gl:uniform-matrix-4fv (get-uniform program "model") (vector model) nil))

    ;; draw
    (gl:bind-vertex-array vao)
    (cond ((or (eql draw-mode :triangle-strip)
               (eql draw-mode :triangles)
               (eql draw-mode :points))
           (gl:point-size 5.0)
           (gl:draw-arrays draw-mode 0 4))
          (t
           (with-sequence-to-gl-array (verts (vector 0 1 3 2 0) :unsigned-int)
             (gl:draw-elements draw-mode verts))))
    (gl:bind-vertex-array 0)))
