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
                                  (vector -0.5 -0.5
                                          0.5 -0.5
                                          0.5 0.5
                                          -0.5 0.5)
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
                    (rotation-center (vec3 0.0 0.0 0.0))
                    (draw-center (vec3 0.0 0.0 0.0))
                    (draw-mode :triangle-strip)
                    (drawer *rect-drawer*))
  (with-accessors ((program program) (vao vao)) drawer
    (use program)

    (gl:uniformfv (get-uniform program "rectColor") color)
    (let ((model (kit.glm:matrix*
                  ;; order of operations is bottom to top since
                  ;; matrix multiplication

                  ;;finally move to POS
                  (kit.glm:translate position)

                  ;; move to draw center
                  (kit.glm:translate* (cfloat (* -1.0
                                                 (x-val draw-center)
                                                 (x-val size)))
                                      (cfloat (* -1.0
                                                 (y-val draw-center)
                                                 (y-val size)))
                                      (cfloat (* -1.0
                                                 (z-val draw-center)
                                                 1.0)))
                  ;; move back
                  (kit.glm:translate* (cfloat (* -1.0
                                                 (x-val rotation-center)
                                                 (x-val size)))
                                      (cfloat (* -1.0
                                                 (y-val rotation-center)
                                                 (y-val size)))
                                      (cfloat (* -1.0
                                                 (z-val rotation-center)
                                                 1.0)))
                  ;; rotate around the z-axis
                  (kit.glm:rotate* 0.0 0.0 (cfloat rotate))
                  ;; move to rotation center
                  (kit.glm:translate* (cfloat (* (x-val rotation-center)
                                                 (x-val size)))
                                      (cfloat (* (y-val rotation-center)
                                                 (y-val size)))
                                      (cfloat (* (z-val rotation-center)
                                                 1.0)))
                  ;; scale first, z axis remain constant since 2d
                  (kit.glm:scale* (cfloat (x-val size)) (cfloat (y-val size)) 0.0))))

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
