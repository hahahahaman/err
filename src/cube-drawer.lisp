(in-package :err)

(defclass cube-drawer (drawer)
  ())

(defmethod initialize-instance :after ((drawer cube-drawer) &key)
  (with-slots (vao) drawer
    (let* ((buffers (gl:gen-buffers 1))
           (vbo (first buffers)))
      (gl:bind-vertex-array vao)
      (gl:bind-buffer :array-buffer vbo)
      (with-cube-verts verts
        (gl:buffer-data :array-buffer :static-draw verts))
      (gl:enable-vertex-attrib-array 0)
      (gl:vertex-attrib-pointer 0 3 :float nil (sizeof* :float 8) 0)
      (gl:bind-buffer :array-buffer 0)
      (gl:bind-vertex-array 0)
      (gl:delete-buffers buffers))))

(defun cube-draw (&key
                    (position (vec3 0.0 0.0 0.0))
                    (size (vec3 1.0 1.0 1.0))
                    (color (vec4 1.0 1.0 1.0 1.0))
                    (rotate (vec3 0.0 0.0 0.0))
                    (draw-mode :triangles)
                    (drawer *cube-drawer*))
  (with-slots (vao program) drawer
    (gl:use-program (id program))

    (let ((model (kit.glm:matrix*
                  (kit.glm:translate position)
                  (kit.glm:rotate rotate)
                  (kit.glm:scale size))))
      (gl:uniform-matrix-4fv (get-uniform program "model")
                             model
                             nil)
      (gl:uniformfv (get-uniform program "color") color))
    (gl:bind-vertex-array vao)
    (gl:draw-arrays draw-mode 0 36)
    (gl:bind-vertex-array 0)))
