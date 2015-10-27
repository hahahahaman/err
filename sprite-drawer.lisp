(in-package :err)

(defclass sprite-drawer (drawer)
  ())

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
                                  (vector 0.0 0.0 0.0 0.0
                                          1.0 0.0 1.0 0.0
                                          0.0 1.0 0.0 1.0
                                          1.0 1.0 1.0 1.0)
                                  :float)
        (gl:buffer-data :array-buffer :static-draw verts))

      (gl:enable-vertex-attrib-array 0)
      (gl:vertex-attrib-pointer 0 4 :float nil (sizeof* :float 4) 0)
      (gl:bind-buffer :array-buffer 0)
      (gl:bind-vertex-array 0)

      ;; clean up
      ;; (gl:free-gl-array verts)
      (gl:delete-buffers (list vbo)))))

(defmethod sprite-draw ((texture2d texture2d)
                        (position vector)
                        &optional
                          (size (kit.glm:vec2 10.0 10.0))
                          (color (kit.glm:vec4 1.0 1.0 1.0 1.0))
                          (rotate 0.0)
                          (drawer *sprite-drawer*))
  (with-accessors ((program program) (vao vao)) drawer
    (use program)

    (gl:uniformfv (get-uniform program "spriteColor") color)
    (let ((model (kit.glm:matrix*
                  ;;finally move to POSITION
                  (kit.glm:translate (kit.glm:vec3 (x-val position)
                                                   (y-val position)
                                                   (z-val position)))
                  ;; move top left to 0.0, 0.0
                  (kit.glm:translate* (cfloat (* 0.5 (x-val size)))
                                      (cfloat (* 0.5 (y-val size)))
                                      0.0)
                  ;; rotate around the z-axis
                  (kit.glm:rotate* 0.0 0.0 (cfloat rotate))
                  ;; move sprite so that its center is at 0.0, 0.0, 0.0
                  (kit.glm:translate* (cfloat (* -0.5 (x-val size)))
                                      (cfloat (* -0.5 (y-val size)))
                                      0.0)
                  ;; scale first, z axis remain constant since 2d
                  (kit.glm:scale (kit.glm:vec3 (x-val size) (y-val size) 0.0)))))

      ;; set model uniform
      (gl:uniform-matrix-4fv (get-uniform program "model") (vector model) nil))

    ;; bind TEXTURE2D
    (gl:active-texture :texture0)
    (bind texture2d)

    ;; draw
    (gl:bind-vertex-array vao)
    (gl:draw-arrays :triangle-strip 0 4)
    (gl:bind-vertex-array 0)
    (gl:bind-texture :texture-2d 0)))
