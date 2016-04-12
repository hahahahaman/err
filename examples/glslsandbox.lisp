(in-package :err-examples)

(defglobal *glsl-drawer*)

(defclass glsl-drawer (err:drawer)
  ())

(defmethod initialize-instance :after ((drawer glsl-drawer) &key)
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

(defun glsl-draw (&key
                    (position (vec3f 0.0 0.0 0.0))
                    (size (vec2f 10.0 10.0))
                    (rotation 0.0)
                    (rotation-center (vec3f 0.0 0.0 0.0))
                    (draw-center (vec3f 0.0 0.0 0.0))
                    (draw-mode :triangle-strip)
                    (drawer *glsl-drawer*))
  (with-slots (program vao) drawer
    (gl:use-program (id program))
    (let ((model (kit.glm:matrix*
                  ;; order of operations is bottom to top since
                  ;; matrix multiplication

                  ;;finally move to POSITION
                  (kit.glm:translate position)

                  ;; move to draw center
                  (kit.glm:translate* (cfloat (* (x-val draw-center)
                                                 (x-val size)
                                                 -1.0))
                                      (cfloat (* (y-val draw-center)
                                                 (y-val size)
                                                 -1.0))
                                      (cfloat (* (z-val draw-center)
                                                 -1.0)))

                  ;; move back from rotation center
                  (kit.glm:translate* (cfloat (* (x-val rotation-center)
                                                 (x-val size)))
                                      (cfloat (* (y-val rotation-center)
                                                 (y-val size)))
                                      (cfloat (* (z-val rotation-center)
                                                 1.0)))
                  ;; rotate around the z-axis
                  (kit.glm:rotate* 0.0 0.0 (cfloat rotation))
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
      (gl:uniform-matrix-4fv (get-uniform program "model") (vector model) nil)
      (gl:uniform-matrix-4fv (get-uniform program "surfaceMatrix")
                             (vector (kit.glm:scale
                                      (vec3f (cfloat (mod (glfw:get-time) 2))
                                             (cfloat (mod (glfw:get-time) 2))
                                             1.0)))
                             nil))

    (gl:bind-vertex-array vao)
    (with-sequence-to-gl-array (verts (vector 0 1 3 2 0) :unsigned-int)
      (gl:draw-elements draw-mode verts))
    (gl:bind-vertex-array 0)))

(defun init-glsl-drawer ()
  (let ((program (make-program (cl-fad::merge-pathnames-as-file
                                *shader-directory*
                                (pathname "glslsandbox/basic.v.glsl"))
                               (cl-fad::merge-pathnames-as-file
                                *shader-directory*
                                (pathname "glslsandbox/spectrum.f.glsl")))))
    (setf *glsl-drawer* (make-instance 'glsl-drawer :program program))
    (load-program "glsl" program)
    (set-program-matrices
     program
     :view nil
     :projection (kit.math:ortho-matrix 0.0 *width* 0.0 *height* -100.0 100.0))
    (gl:use-program (id program))
    
    (gl:uniformf (get-uniform program "time") 0.0)
    (gl:uniformfv (get-uniform program "mouse") (vec2f (cfloat *cursor-x*)
                                                       (cfloat (- *height* *cursor-y*))))
    (gl:uniformfv (get-uniform program "resolution") (vec2f (cfloat *width*)
                                                            (cfloat *height*)))))

(defun glsl-init()
  (initialize-shaders *shader-directory*)
  (init-glsl-drawer))

(defun glsl-input()
  (when (key-pressed-p :escape)
    (close-window))
  (when (and (key-pressed-p :left-alt)
             (key-action-p :r :press)) 
    (add-event :code (glsl-init))))

(defrender glsl-render 60.0
  (gl:enable :blend :depth-test)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear-color 0.0 0.0 0.0 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (glsl-draw :draw-center (vec3f -0.5 -0.5 0.0)
             :size (vec2f (cfloat *width*) (cfloat *height*))))

(defupdate glsl-update 120.0
  (let ((program (get-program "glsl")))
    (gl:use-program (id program))
    (gl:uniformf (get-uniform program "time") (glfw:get-time))
    (gl:uniformfv (get-uniform program "mouse")
                  (vec2f (cfloat *cursor-x*)
                         (cfloat (- *height* *cursor-y*))))))

(defun glsl-cleanup())

(defun glslsandbox ()
  (setf *width* 1024
        *height* 768)
  (err-run "glsl"
           :init-code (glsl-init)
           :input-code (glsl-input)
           :render-code (glsl-render)
           :update-code (glsl-update)
           :cleanup-code (glsl-cleanup)))
