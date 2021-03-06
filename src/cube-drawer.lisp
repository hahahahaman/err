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
                    (position (vec3f 0.0 0.0 0.0))
                    (size (vec3f 1.0 1.0 1.0))
                    (color (vec4f 1.0 1.0 1.0 1.0))
                    (rotation (vec3f 0.0 0.0 0.0))
                    (rotation-center (vec3f 0.0 0.0 0.0))
                    (draw-center (vec3f 0.0 0.0 0.0))
                    (draw-mode :triangles)
                    (drawer *cube-drawer*))
  (with-slots (vao program) drawer
    (gl:use-program (id program))

    (let ((model (kit.glm:matrix*

                  ;; move into position
                  (kit.glm:translate position)

                  ;; move to draw center
                  (kit.glm:translate (vec3f-mul size (vec3f* draw-center -1.0)))

                  ;; move back from rotation center
                  (kit.glm:translate (vec3f-mul rotation-center size))

                  ;; perform rotation
                  (kit.glm:rotate rotation)

                  ;; move to rotation center
                  (kit.glm:translate (vec3f* (vec3f-mul rotation-center size)
                                             -1.0))

                  ;; scale first
                  (kit.glm:scale size))))

      (gl:uniform-matrix-4fv (get-uniform program "model") model nil)
      (gl:uniformfv (get-uniform program "color") color))
    (gl:bind-vertex-array vao)
    (gl:draw-arrays draw-mode 0 36)
    (gl:bind-vertex-array 0)))
