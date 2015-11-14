;;; flocking behaviour
(in-package :err-examples)

#|
1. seperation - move away, prevent crowding
2. alignment - move with average velocity of close members
3. cohesion - move towards average position of members
|#

(defun init-managers ()
  (setf *program-manager* (make-instance 'program-manager)
        *texture-manager* (make-instance 'texture-manager)
        *font-manager* (make-instance 'font-manager)))

(defun flock-init ()
  (let* ((proj-dir (asdf:system-source-directory :err-examples))
         (shader-dir (if *executable*
                         #p"./data/shaders/"
                         (merge-pathnames #p"examples/data/shaders/" proj-dir)))
         (font-dir (if *executable*
                       #p"./data/fonts/"
                       (merge-pathnames #p"examples/data/fonts/" proj-dir)))
         (text-program (make-program (merge-pathnames #p"text.v.glsl" shader-dir)
                                     (merge-pathnames #p"text.f.glsl" shader-dir)))
         (cube-program (make-program (merge-pathnames #p"cube.v.glsl" shader-dir)
                                     (merge-pathnames #p"cube.f.glsl" shader-dir))))
    (init-managers)
    (setf *text-drawer* (make-instance 'text-drawer :program text-program)
          *cube-drawer* (make-instance 'cube-drawer :program cube-program)
          *camera* (make-instance 'camera :position (vec3 0.0 0.0 3.0)))
    (load-font "sans24" (merge-pathnames #p"DejaVuSans.ttf" font-dir) 24)

    ;; set cube program matrices
    (let ((view (get-view-matrix *camera*))
          (proj (kit.glm:perspective-matrix
                 (kit.glm:deg-to-rad (zoom *camera*))
                 (cfloat (/ *width* *height*))
                 0.1 100.0)))
      (gl:use-program (id cube-program))
      (gl:uniform-matrix-4fv (get-uniform cube-program "view") view nil)
      (gl:uniform-matrix-4fv (get-uniform cube-program "projection") proj nil))

    ;; set text program matrices
    (let ((proj (kit.glm:ortho-matrix 0.0 (cfloat *width*)
                                      0.0 (cfloat *height*)
                                      -1.0 1.0)))
      (gl:use-program (id text-program))
      (gl:uniform-matrix-4fv (get-uniform text-program "projection") proj nil))))

(defun flock-handle-input ()
  (when (key-pressed-p :escape)
    (close-window)))

(defun flock-render ()
  (gl:enable :blend :depth-test)
  (gl:disable :cull-face)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear-color 0.0 0.0 0.0 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (cube-draw :position (vec3 0.0 0.0 -10.0)
             :color (vec4 0.0 0.4 0.2 1.0)
             :rotate (vec3 0.4 0.4 0.0))
  (text-draw "testing" (get-font "sans24")
             :position (vec3 200.0 200.0 0.0)))

(defun flock-update ())
(defun flock-cleanup-code ())

(defmacro start-flock ()
  `(err:run "flock"
            :init-code (flock-init)
            :input-code (flock-handle-input)
            :render-code (flock-render)
            :update-code (flock-update)
            :cleanup-code (flock-cleanup-code)))

(defun flock ()
  (start-flock))
