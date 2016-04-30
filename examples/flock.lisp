;;; flocking behaviour
(in-package :err-examples)

#|
1. seperation - move away from close members, prevent crowding
2. alignment - move with average velocity of close members
3. cohesion - move towards average position of members
|#

(defglobal *boundary* (cons -20.0 20.0))

(defun create-boid ()
  (let* ((lo (car *boundary*))
         (hi (cdr *boundary*))
         (size (vec3f 1.0 1.0 1.0))
         (vel-range 10.0)
         (neg-vel-range (- vel-range))
         (pos (vec3f (random-in-range lo hi)
                     (random-in-range lo hi)
                     (random-in-range lo hi)))
         (vel (vec3f (random-in-range neg-vel-range vel-range)
                     (random-in-range neg-vel-range vel-range)
                     (random-in-range neg-vel-range vel-range)))
         (accel (vec3f 0.0 0.0 0.0)))
    (add-entity (map (:pos pos)
                     (:size size)
                     (:vel vel)
                     (:accel accel)
                     (:n-proxy 0)))))

(defun init-entities ()
  (iter (for i from 0 below 70)
    (create-boid)))

(defun flock-init ()
  (init-shaders)
  (init-entities))

(let ((move-camera? nil))
  (defun flock-handle-input ()
    (when (key-pressed-p :escape)
      (close-window))
    (when (key-action-p :r :press)
      (initialize-globals)
      (flock-init))

    (when (key-action-p :space :press)
      (setf move-camera? (not move-camera?)))

    (when move-camera?
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
        (gl:uniform-matrix-4fv (get-uniform program "projection") proj nil)))))

(defrender flock-render 200.0
  (gl:enable :blend :depth-test)
  (gl:disable :cull-face)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear-color 0.0 0.0 0.0 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (cube-draw :position (vec3f 0.0 0.0 0.0)
             :color (vec4f 0.1 0.4 0.7 1.0)
             :rotation (vec3f 0.0 0.0 0.0))

  ;; draw entities
  (do-map (id comps *entities*)
    (declare (ignore id))
    (cube-draw :position (@ comps :pos)
               :size (@ comps :size)))

  ;; fps
  (text-draw (format nil "~4f" (cfloat (average-fps)))
             (get-font "sans24")
             :position (vec2f 1.0 3.0)
             :scale (vec2f 0.7 0.7)
             :draw-center (vec3f -0.5 -0.5 0.0)))


(declaim (ftype (function (vec3f vec3f) vec3f) vec3f-to))
(defun vec3f-to (a b)
  "vector from a to b"
  (declare (optimize (speed 3) (safety 0)))
  (kit.glm:vec- b a))

(declaim (ftype (function (vec3f vec3f) single-float) vec3f-distance))
(defun vec3f-distance (a b)
  (declare (optimize (speed 3) (safety 0)))
  (kit.glm:vec-length (vec3f-to a b)))

(defupdate flock-update 200.0
  (let ((boid-vecs (make-array 0 :element-type 'vec3f
                                 :fill-pointer 0
                                 :adjustable t))
        (max-accel-mag 20.0)
        (max-vel-mag 10.0))
    (do-map (id comps *entities*)
      ;; seperation
      ;; alignment
      ;; cohesion

      (let ((n-proximity 1)
            (seperate-vec (vec3f 0.0 0.0 0.0))
            (align-vec (vec3f 0.0 0.0 0.0))
            (cohesion-vec (vec3f 0.0 0.0 0.0))
            (pos (@ comps :pos))
            ;; (accel (@ comps :accel))
            ;; (short-range-sense 3.0)
            (long-range-sense 8.0))

        ;; find nearby boids and get basic behaviour vectors
        (do-map (oid ocomps *entities*)
          (unless (= id oid)
            (let ((opos (@ ocomps :pos))
                  (ovel (@ ocomps :vel)))
              (when (< (vec3f-distance pos opos) long-range-sense)
                (incf n-proximity)
                (setf seperate-vec (kit.glm:vec+ seperate-vec (vec3f-to pos opos))
                      align-vec (kit.glm:vec+ align-vec ovel)
                      cohesion-vec (kit.glm:vec+ cohesion-vec opos))))))
        (setf
         ;; opposite of average direction towards others
         seperate-vec (kit.glm:vec* seperate-vec -1.0)
         seperate-vec (kit.glm:normalize seperate-vec)
         seperate-vec (kit.glm:vec* seperate-vec (* max-accel-mag 0.1))

         ;; average velocity of others
         align-vec (kit.glm:vec/ align-vec (cfloat n-proximity))
         align-vec (kit.glm:normalize seperate-vec)
         align-vec (kit.glm:vec* align-vec (* max-accel-mag 0.15))

         ;; vector to average position of others
         cohesion-vec (vec3f-to pos (kit.glm:vec/ cohesion-vec (cfloat n-proximity)))
         cohesion-vec (kit.glm:normalize cohesion-vec)
         cohesion-vec (kit.glm:vec* cohesion-vec (* max-accel-mag 1.09)))

        (vector-push-extend (if (> n-proximity 0)
                                (kit.glm:vec+ cohesion-vec
                                              (kit.glm:vec+ seperate-vec
                                                            align-vec))
                                (@ comps :accel))
                            boid-vecs)))

    (add-event
     :code
     (let ((boid-counter 0))
       (do-map (id components *entities*)
         (let* ((pos (@ components :pos))
                (vel (@ components :vel))
                (accel (aref boid-vecs boid-counter))
                (timestep (timer-end update-timer))
                (a/2 (kit.glm:vec* accel (* 0.5 timestep)))
                (min-bound (car *boundary*))
                (max-bound (cdr *boundary*)))

           ;;symplectic euler integration
           ;; v += a/2 * dt
           ;; p += v * dt
           ;; v += a/2 * dt
           (setf vel (vec3f+ vel a/2)
                 pos (vec3f+ pos (vec3f* vel timestep))
                 vel (vec3f+ vel a/2))

           ;; constrain velocity
           (when (< max-vel-mag (vec3f-length vel))
             (setf vel (vec3f* (vec3f-normalize vel) max-vel-mag)))

           ;; update new values
           (setf components
                 (-> components
                     (with :accel accel)
                     (with :vel vel)
                     (with :pos pos)))

           ;; bound boids within an area
           (let ((pos-copy (copy-seq (@ components :pos)))
                 (vel-copy (copy-seq (@ components :vel))))
             (iter (for p in-vector pos)
               (for v in-vector vel)
               (for i from 0)
               (cond ((< p min-bound)
                      (setf (aref pos-copy i) max-bound
                            (aref vel-copy i) (- v))
                      (includef components :pos pos-copy)
                      ;; (includef components :vel vel-copy)
                      )
                     ((> p max-bound)
                      (setf (aref pos-copy i) min-bound
                            (aref vel-copy i) (- v))
                      (includef components :pos pos-copy)
                      ;; (includef components :vel vel-copy)
                      ))))

           ;; change in entities
           (includef *entities* id components))

         (incf boid-counter))))))

(defun flock-cleanup-code ())

(defmacro start-flock ()
  `(err-run "flock"
            :init-code (flock-init)
            :input-code (flock-handle-input)
            :render-code (flock-render)
            :update-code (flock-update)
            :cleanup-code (flock-cleanup-code)))

(defun flock ()
  (start-flock))
