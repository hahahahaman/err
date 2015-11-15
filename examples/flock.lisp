;;; flocking behaviour
(in-package :err-examples)

#|
1. seperation - move away from close members, prevent crowding
2. alignment - move with average velocity of close members
3. cohesion - move towards average position of members
|#

(defglobal *boundary* (cons -50.0 50.0))
(defglobal *move-camera?* t)

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
                                     (merge-pathnames #p"cube.f.glsl" shader-dir))))
    (init-managers)
    (setf *text-drawer* (make-instance 'text-drawer :program text-program)
          *cube-drawer* (make-instance 'cube-drawer :program cube-program)
          *camera* (make-instance 'camera :position (vec3 0.0 0.0 50.0)))

    (load-program "text" text-program)
    (load-program "cube" cube-program)

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

    ;; set text program matrices
    (let ((proj (kit.glm:ortho-matrix 0.0 (cfloat *width*)
                                      0.0 (cfloat *height*)
                                      -1.0 1.0)))
      (gl:use-program (id text-program))
      (gl:uniform-matrix-4fv (get-uniform text-program "projection") proj nil))))

(defun init-entities ()
  (iter (for i from 0 below 50)
    (let* ((lo (car *boundary*))
           (hi (cdr *boundary*))
           (size (vec3 1.0 1.0 1.0))
           (vel-range 10.0)
           (neg-vel-range (- vel-range))
           (pos (vec3 (random-in-range lo (- hi (x-val size)))
                      (random-in-range (+ lo (y-val size)) hi)
                      (random-in-range lo (- hi (z-val size)))))
           (vel (vec3 (random-in-range neg-vel-range vel-range)
                      (random-in-range neg-vel-range vel-range)
                      (random-in-range neg-vel-range vel-range)))
           (accel (vec3 0.0 0.0 0.0)))
      (add-entity (map (:pos pos)
                       (:size size)
                       (:vel vel)
                       (:accel accel))))))

(defun flock-init ()
  (init-shaders)
  (init-entities))

(defun flock-handle-input ()
  (when (key-pressed-p :escape)
    (close-window))
  (when (key-action-p :r :press)
    (initialize-globals)
    (flock-init))

  (when (key-action-p :space :press)
    (setf *move-camera?* (not *move-camera?*)))

  (when *move-camera?*
    (when *cursor-callback-p*
      (let ((x-offset (cfloat (- *cursor-x* *last-x*)))
            (y-offset (cfloat (- *last-y* *cursor-y*))))
        (process-rotation-movement *camera* x-offset y-offset)))

    (when *scroll-callback-p*
      (process-scroll-movement *camera* (cfloat *scroll-y*))) 

    (when (key-pressed-p :w)
      (process-direction-movement *camera* +forward+ *dt*))
    (when (key-pressed-p :s)
      (process-direction-movement *camera* +backward+ *dt*))
    (when (key-pressed-p :a)
      (process-direction-movement *camera* +left+ *dt*))
    (when (key-pressed-p :d)
      (process-direction-movement *camera* +right+ *dt*))

    (let ((program (get-program "cube"))
          (view (get-view-matrix *camera*))
          (proj (kit.glm:perspective-matrix (kit.glm:deg-to-rad (zoom *camera*))
                                            (cfloat (/ *width* *height*))
                                            0.1 1000.0)))
      (gl:use-program (id program))
      (gl:uniform-matrix-4fv (get-uniform program "view") view nil)
      (gl:uniform-matrix-4fv (get-uniform program "projection") proj nil)))

  (setf *last-x* *cursor-x*
        *last-y* *cursor-y*)
  (setf *cursor-callback-p* nil
        *scroll-callback-p* nil))


(defun flock-render ()
  (gl:enable :blend :depth-test)
  (gl:disable :cull-face)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear-color 0.0 0.0 0.0 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (cube-draw :position (vec3 0.0 0.0 10.0)
             :color (vec4 0.0 0.4 0.2 1.0)
             :rotate (vec3 0.0 0.0 0.0))

  ;; draw entities
  (do-map (id comps *entities*)
    (declare (ignore id))
    (cube-draw :position (@ comps :pos)
               :size (@ comps :size)))

  ;; fps
  (text-draw (format nil "~4f" (cfloat (average-fps)))
             (get-font "sans24")
             :position (vec2 1.0 3.0)
             :scale (vec2 0.7 0.7)))

(defglobal *move-timer* (make-timer))
(defglobal *move-timestep* (/ 1.0 60.0))
(defun flock-update ()
  (timer-update *move-timer*)
  (iter (while (>= (timer-time *move-timer*) *move-timestep*))
    (do-map (id comps *entities*)
      ;; seperation
      ;; alignment
      ;; cohesion

      ;;sympletic euler integration
      (add-event
       :code
       ;; v += a/2 * dt
       ;; p += v * dt
       ;; v += a/2 * dt
       (let* ((pos (@ comps :pos))
              (vel (@ comps :vel))
              (accel (@ comps :accel))
              (a/2 (kit.glm:vec* accel (* 0.5 *move-timestep*))))
         (with! *entities* id
                (-> comps
                    (with :vel (kit.glm:vec+ vel a/2))
                    (with :pos (kit.glm:vec+
                                pos (kit.glm:vec* vel *move-timestep*)))
                    (with :vel (kit.glm:vec+ vel a/2))))))
      ;; move to opposite side if out of bounds
      (add-event
       :code
       (let* ((pos (@ comps :pos))
              (pos-copy (copy-seq pos))
              (min-bound (car *boundary*))
              (max-bound (cdr *boundary*)))
         (iter (for p in-vector pos)
           (for i from 0)
           (cond ((< p min-bound)
                  (setf (aref pos-copy i) max-bound)
                  (with! *entities* id (with comps :pos pos-copy)))
                 ((> p max-bound)
                  (setf (aref pos-copy i) min-bound)
                  (with! *entities* id
                         (with comps :pos pos-copy))))))))

    (decf (timer-time *move-timer*) *move-timestep*)))

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
