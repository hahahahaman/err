(in-package :err-examples)

(defglobal *platformer-player* nil)
(defglobal *platformer-level* (empty-seq))

(defun rects-collide-p (x y w h ox oy ow oh)
  "Rectangle collision check based on bottom left position of the rectangle."
  (not (or (< (+ ox ow) x)
           (> ox (+ x w))
           (< oy (- y h))
           (> (- oy oh) y))))

;; (defun move-entity (entity)
;;   "=> ENTITY (MAP)
;; Moves the entity, checking collision along the way."
;;   )

(defun platformer-init ()
  (init-shaders)

  ;; player
  (setf *platformer-player* (add-entity (map (:x 0.0)
                                             (:y 0.0)
                                             (:w 32)
                                             (:h 32)
                                             (:velx 0.0)
                                             (:vely 0.0)
                                             (:accelx 0.0)
                                             (:accely 0.0)
                                             (:max-velx 10.0)
                                             (:max-vely 10.0))))
  ;;platforms
  (setf *platformer-level* (with-last *platformer-level* (map (:x 0.0)
                                                              (:y 0.0)
                                                              (:w 100.0)
                                                              (:h 100.0)))))

(defun platformer-handle-input ()
  (when (key-action-p :escape :press)
    (glfw:set-window-should-close))

  ;; player input
  (add-event
   :code
   (let* ((player (@ *entities* *platformer-player*))
          (w-p (key-pressed-p :w))
          (s-p (key-pressed-p :s))
          (a-p (key-pressed-p :a))
          (d-p (key-pressed-p :d))
          (accel-rate 2.0))

     ;; handle up and down movement
     (when w-p
       (with! player :accely accel-rate))
     (when s-p
       (with! player :accely (- accel-rate)))
     (when (and w-p s-p)
       (with! player :accely 0.0))
     (when (not (or w-p s-p))
       (cond ((< (abs (@ player :vely)) 2.0)
              (with! player :vely 0.0)
              (with! player :accely 0.0))
             (t (with! player :accely (* (- (signum (@ player :vely)))
                                         accel-rate 5.0)))))

     ;; left and right movement
     (when d-p
       (with! player :accelx accel-rate))
     (when a-p
       (with! player :accelx (- accel-rate)))
     (when (and a-p d-p)
       (with! player :accelx 0.0))
     (when (not (or d-p a-p))
       (cond ((< (abs (@ player :velx)) 2.0)
              (with! player :velx 0.0)
              (with! player :accelx 0.0))
             (t (with! player :accelx (* (- (signum (@ player :velx)))
                                         accel-rate 5.0)))))

     (with! *entities* *platformer-player* player))))

(let ((update-timer (make-timer :end (/ 1.0 70.0))))
  (defun platformer-update ()
    (timer-update update-timer)

    (iter (while (timer-ended-p update-timer))
      (timer-keep-overflow update-timer)

      (add-event
       :code
       (do-map (id components *entities*)
         (let* ((x (@ components :x))
                (y (@ components :y))
                (velx (@ components :velx))
                (vely (@ components :vely))
                (max-velx (@ components :max-velx))
                (max-vely (@ components :max-vely))
                (accelx (@ components :accelx))
                (accely (@ components :accely))
                ;; (max-accelx (@ components :max-accelx))
                ;; (max-accely (@ components :max-accely))
                (dt (timer-end update-timer)) ;; timestep
                (accelx/2 (* accelx 0.5 dt))
                (accely/2 (* accely 0.5 dt)))

           ;; https://www.niksula.hut.fi/~hkankaan/Homepages/gravity.html
           ;; symplectic euler integration
           ;; v += a/2 * dt
           ;; p += v * dt
           ;; v += a/2 * dt

           (incf velx accelx/2)
           (incf vely accely/2)

           ;; bound velocity once, where it affects the position
           (setf velx (clampf velx (- max-velx) max-velx)
                 vely (clampf vely (- max-vely) max-vely))

           (incf x (* velx dt))
           (incf y (* vely dt))

           (incf velx accelx/2)
           (incf vely accely/2)

           ;; update values
           (setf components
                 (-> components
                     (with :velx velx)
                     (with :vely vely)
                     (with :x x)
                     (with :y y))))

         ;; update the entity
         (with! *entities* id components))))))

(let ((render-timer (make-timer :end (/ 1.0 60.0))))
  (defun platformer-render ()
    (timer-update render-timer)
    (when (timer-ended-p render-timer)

      (gl:enable :blend :depth-test)
      ;; (gl:disable :cull-face)
      (gl:blend-func :src-alpha :one-minus-src-alpha)
      (gl:clear-color 0.0 0.0 0.0 1.0)
      (gl:clear :color-buffer-bit :depth-buffer-bit)

      (let ((player (@ *entities* *platformer-player*)))
        (cube-draw :position (vec3f (@ player :x) (@ player :y) 0.0))
        (text-draw (format nil "~6f ~6f ~6f ~6f ~6f ~6f"
                           (@ player :x) (@ player :y)
                           (@ player :velx) (@ player :vely)
                           (@ player :accelx) (@ player :accely))
                   (get-font "sans24")
                   :position (vec2f 3.0 (- (cfloat *height*) 20.0))
                   :scale (vec2f 1.0 1.0)))

      ;; fps
      (text-draw (format nil "~4f" (cfloat (average-fps)))
                 (get-font "sans24")
                 :position (vec2f 1.0 3.0)
                 :scale (vec2f 0.7 0.7))

      (timer-reset render-timer))))

(defun platformer-cleanup ())

(defmacro platformer-start ()
  `(err:run "platformer"
            :init-code (platformer-init)
            :input-code (platformer-handle-input)
            :render-code (platformer-render)
            :update-code (platformer-update)
            :cleanup-code (platformer-cleanup)))

(defun platformer ()
  (platformer-start))
