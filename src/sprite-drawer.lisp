(in-package :err)

(defclass sprite-drawer (drawer)
  ((current-texture-id
    :accessor current-texture-id
    :initarg :current-texture-id))
  (:default-initargs
   :current-texture-id nil))

(defmethod initialize-instance :after ((drawer sprite-drawer) &key)
  (with-slots (vao) drawer
    ;; delete qua-vao after garbage collected

    (let ((vbo (car (gl:gen-buffers 1))))
      ;; use vao from drawer
      (gl:bind-vertex-array vao)

      ;; upload buffer data from vertex buffer object
      (gl:bind-buffer :array-buffer vbo)
      (with-sequence-to-gl-array (verts
                                  ;;      Pos     Tex
                                  (vector -0.5 -0.5 0.0 0.0
                                          0.5 -0.5 1.0 0.0
                                          -0.5 0.5 0.0 1.0
                                          0.5 0.5 1.0 1.0)
                                  :float)
        (gl:buffer-data :array-buffer :static-draw verts))

      (gl:enable-vertex-attrib-array 0)
      (gl:vertex-attrib-pointer 0 4 :float nil (sizeof* :float 4) 0)
      (gl:bind-buffer :array-buffer 0)
      (gl:bind-vertex-array 0)

      ;; clean up
      ;; (gl:free-gl-array verts)
      (gl:delete-buffers (list vbo)))))

(defun sprite-draw (texture2d
                    &key
                      (position (vec3 0.0 0.0 0.0))
                      (size (kit.glm:vec2 10.0 10.0))
                      (color (kit.glm:vec4 1.0 1.0 1.0 1.0))
                      (rotate 0.0)
                      (rotation-center (vec3f 0.0 0.0 0.0))
                      (draw-center (vec3f 0.0 0.0 0.0))
                      (drawer *sprite-drawer*))
  (with-slots (program vao current-texture-id) drawer
    (use program)

    (gl:uniformfv (get-uniform program "spriteColor") color)
    (let ((model (kit.glm:matrix*
                  ;; order of operations is bottom to top since
                  ;; matrix multiplication

                  ;;finally move to POSITION
                  (kit.glm:translate position)

                  ;; move to draw center
                  (kit.glm:translate* (cfloat (* (x-val draw-center)
                                                 (x-val size)))
                                      (cfloat (* (y-val draw-center)
                                                 (y-val size)))
                                      (cfloat (* (z-val draw-center)
                                                 1.0)))

                  ;; move back from rotation center
                  (kit.glm:translate* (cfloat (* (x-val rotation-center)
                                                 (x-val size)))
                                      (cfloat (* (y-val rotation-center)
                                                 (y-val size)))
                                      (cfloat (* (z-val rotation-center)
                                                 1.0)))
                  ;; rotate around the z-axis
                  (kit.glm:rotate* 0.0 0.0 (cfloat rotate))
                  ;; move to rotation center
                  (kit.glm:translate* (cfloat (* -1.0
                                                 (x-val rotation-center)
                                                 (x-val size)))
                                      (cfloat (* -1.0
                                                 (y-val rotation-center)
                                                 (y-val size)))
                                      (cfloat (* -1.0
                                                 (z-val rotation-center)
                                                 1.0)))
                  ;; scale first, z axis remain constant since 2d
                  (kit.glm:scale* (cfloat (x-val size)) (cfloat (y-val size)) 0.0))))

      ;; set model uniform
      (gl:uniform-matrix-4fv (get-uniform program "model") (vector model) nil))

    ;; bind TEXTURE2D unless it is already bound
    (unless (eql (id texture2d) current-texture-id)
      (gl:active-texture :texture0)
      (gl:bind-texture :texture-2d (id texture2d))
      (setf current-texture-id (id texture2d)))

    ;; draw
    (gl:bind-vertex-array vao)
    (gl:draw-arrays :triangle-strip 0 4)
    (gl:bind-vertex-array 0)
    ;; (gl:bind-texture :texture-2d 0)
    ))
