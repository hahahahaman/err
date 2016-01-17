#|
Utility functions
|#
(in-package :err-examples)

(defun init-managers ()
  (setf *program-manager* (make-instance 'program-manager)
        *texture-manager* (make-instance 'texture-manager)
        *font-manager* (make-instance 'font-manager)))

(defun init-shaders ()
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
                                     (merge-pathnames #p"cube.f.glsl" shader-dir)))
         (rect-program (make-program (merge-pathnames #p"rect.v.glsl" shader-dir)
                                     (merge-pathnames #p"rect.f.glsl" shader-dir)))
         (sprite-program (make-program (merge-pathnames #p"sprite.v.glsl" shader-dir)
                                       (merge-pathnames #p"sprite.f.glsl" shader-dir))))
    (init-managers)
    (setf *text-drawer* (make-instance 'text-drawer :program text-program)
          *cube-drawer* (make-instance 'cube-drawer :program cube-program)
          *rect-drawer* (make-instance 'rect-drawer :program rect-program)
          *sprite-drawer* (make-instance 'sprite-drawer :program sprite-program)
          *camera* (make-instance 'camera :position (vec3 0.0 0.0 100.0)
                                          :movement-speed 10.0))

    (load-program "text" text-program)
    (load-program "cube" cube-program)
    (load-program "rect" rect-program)
    (load-program "sprite" sprite-program)

    (load-font "sans24" (merge-pathnames #p"DejaVuSans.ttf" font-dir) 24)

    ;; set cube program matrices
    (let ((view (get-view-matrix *camera*))
          (proj (kit.glm:perspective-matrix
                 (kit.glm:deg-to-rad (zoom *camera*))
                 (cfloat (/ *width* *height*))
                 0.1 1000.0)))
      (gl:use-program (id cube-program))
      (gl:uniform-matrix-4fv (get-uniform cube-program "view") view nil)
      (gl:uniform-matrix-4fv (get-uniform cube-program "projection") proj nil))

    ;; set rect program matrices
    (let ((view (get-view-matrix *camera*))
          (proj (kit.glm:perspective-matrix
                 (kit.glm:deg-to-rad (zoom *camera*))
                 (cfloat (/ *width* *height*))
                 0.1 1000.0)))
      (gl:use-program (id rect-program))
      (gl:uniform-matrix-4fv (get-uniform rect-program "view") view nil)
      (gl:uniform-matrix-4fv (get-uniform rect-program "projection") proj nil))

    ;; set sprite program matrices
    (let ((view (get-view-matrix *camera*))
          (proj (kit.glm:perspective-matrix
                 (kit.glm:deg-to-rad (zoom *camera*))
                 (cfloat (/ *width* *height*))
                 0.1 1000.0)))
      (gl:use-program (id sprite-program))
      (gl:uniform-matrix-4fv (get-uniform sprite-program "view") view nil)
      (gl:uniform-matrix-4fv (get-uniform sprite-program "projection") proj nil))

    ;; set text program matrices
    (let ((proj (kit.glm:ortho-matrix 0.0 (cfloat *width*)
                                      0.0 (cfloat *height*)
                                      -1.0 1.0)))
      (gl:use-program (id text-program))
      (gl:uniform-matrix-4fv (get-uniform text-program "projection") proj nil))))

(defun update-view-matrices ()
  (let ((cube-program (get-program "cube"))
        (rect-program (get-program "rect"))
        (sprite-program (get-program "sprite"))
        (view (get-view-matrix *camera*))
        (proj (kit.glm:perspective-matrix (kit.glm:deg-to-rad (zoom *camera*))
                                          (cfloat (/ *width* *height*))
                                          0.1 1000.0)))
    (gl:use-program (id cube-program))
    (gl:uniform-matrix-4fv (get-uniform cube-program "view") view nil)
    (gl:uniform-matrix-4fv (get-uniform cube-program "projection") proj nil)

    (gl:use-program (id rect-program))
    (gl:uniform-matrix-4fv (get-uniform rect-program "view") view nil)
    (gl:uniform-matrix-4fv (get-uniform rect-program "projection") proj nil)

    (gl:use-program (id sprite-program))
    (gl:uniform-matrix-4fv (get-uniform sprite-program "view") view nil)
    (gl:uniform-matrix-4fv (get-uniform sprite-program "projection") proj nil)))

(defun rects-collide-p (x y w h ox oy ow oh)
  "Rectangle collision check based on bottom left position of the rectangle."
  (not (or (< (+ ox ow) x)
           (> ox (+ x w))
           (< oy (- y h))
           (> (- oy oh) y))))
