;;; flocking behaviour

#|
1. seperation
2. alignment
3. cohesion
|#

(defun init-managers ()
  (setf *program-manager* (make-instance 'program-manager)
        *texture-manager* (make-instance 'texture-manager)
        *font-manager* (make-instance 'font-manager)))
(defun flock-init ()
  (let* ((proj-dir (asdf:system-source-directory :err-examples))
         (shader-dir (merge-pathnames #p"examples/data/shaders/" proj-dir))
         (text-program (make-program (merge-pathnames #p"text.v.glsl" shader-dir)
                                     (merge-pathnames #p"text.f.glsl" shader-dir)))
         (cube-program (make-program (merge-pathnames #p"cube.v.glsl" shader-dir)
                                     (merge-pathnames #p"cube.f.glsl" shader-dir))))
    ))
(defun flock-handle-input ())
(defun flock-render ())
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
