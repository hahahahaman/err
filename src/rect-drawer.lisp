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
                                  (vector
                                   -0.5 -0.5
                                   0.5 -0.5
                                   -0.5 0.5
                                   0.5 0.5)
                                  :float)
        (gl:buffer-data :array-buffer :static-draw verts))

      (gl:enable-vertex-attrib-array 0)
      (gl:vertex-attrib-pointer 0 2 :float nil (sizeof* :float 2) 0)

      (gl:bind-buffer :array-buffer 0)
      (gl:bind-vertex-array 0)

      (gl:delete-buffers buffers))))

(defun rect-draw (&key
                    (position (vec3f 0.0 0.0 0.0))
                    (size (vec2f 10.0 10.0))
                    (color (vec4f 1.0 1.0 1.0 1.0))
                    (rotate 0.0)
                    (rotation-center (vec3f 0.0 0.0 0.0))
                    (draw-center (vec3f 0.0 0.0 0.0))
                    (draw-mode :triangle-strip)
                    (drawer *rect-drawer*))
  "Draws a rectangle on screen.
POSITION is a vec3 with the elements representing the x,y,z coordinate.
SIZE is a vec2 of the width and height.
COLOR is a vec4 of RGBA.
ROTATE is the angle in radians of rotation around the rotation center.
ROTATION-CENTER is the point from which rotation occurs relative to the object,
so -0.5,-0.5, 0.0 means rotate from the bottom left of the rect.
DRAW-CENTER is the point from which drawing occurs relative to the object.
So, -0.5, 0.5, 0.0 means draw from the top left of the rect.
DRAW-MODE is the gl mode to draw the rect.
DRAWER is a rect-drawer object that by default is the global rect-drawer."

  (with-accessors ((program program) (vao vao)) drawer
    (use program)

    (gl:uniformfv (get-uniform program "rectColor") color)
    (let ((model (kit.glm:matrix*
                  ;; order of operations is bottom to top since
                  ;; matrix multiplication

                  ;;finally move to POS
                  (kit.glm:translate position)

                  ;; move to draw center
                  (kit.glm:translate* (cfloat (* (x-val draw-center)
                                                 (x-val size)))
                                      (cfloat (* (y-val draw-center)
                                                 (y-val size)))
                                      (cfloat (* (z-val draw-center)
                                                 1.0)))
                  ;; move back
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

    ;; draw
    (gl:bind-vertex-array vao)
    (cond ((or (eql draw-mode :points))
           (gl:draw-arrays draw-mode 0 4))
          (t
           ;; element buffer object only needs a gl-array in cl-opengl
           (with-sequence-to-gl-array (verts (vector 0 1 3 2 0) :unsigned-int)
             (gl:draw-elements draw-mode verts))))
    (gl:bind-vertex-array 0)))
