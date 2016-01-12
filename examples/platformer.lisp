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
  (initialize-globals)
  (init-shaders)

  ;; player
  (setf *platformer-player* (add-entity (map (:x 10.0)
                                             (:y 10.0)
                                             (:z 0.0)
                                             (:w 5.0)
                                             (:h 5.0)
                                             (:color (vec4f 1.0 0.0 0.0 1.0))
                                             (:velx 0.0)
                                             (:vely 0.0)
                                             (:accelx 0.0)
                                             (:accely 0.0)
                                             (:max-velx 10.0)
                                             (:max-vely 10.0))))
  ;;platforms
  (setf *platformer-level* (with-last *platformer-level* (map (:x 0.0)
                                                              (:y 0.0)
                                                              (:z -1.0)
                                                              (:w 100.0)
                                                              (:h 5.0)))))

(let ((restart nil))
  (defun set-restart-window (&optional (value t))
    (setf restart value))
  (defun restart-window-p ()
    restart)
  (defun restart-window ()
    (setf restart nil)))

(defun platformer-handle-input ()
  (when (key-action-p :escape :press)
    (glfw:set-window-should-close))

  (when (and (key-pressed-p :left-control)
             (key-pressed-p :left-alt)
             (key-action-p :r :press))
    (platformer-init))

  ;; player input
  (add-event
   :code
   (let* ((player (@ *entities* *platformer-player*))
          (up-p (or (key-pressed-p :w) (key-pressed-p :up)))
          (down-p (or (key-pressed-p :s) (key-pressed-p :down)))
          (left-p (or (key-pressed-p :a) (key-pressed-p :left)))
          (right-p (or (key-pressed-p :d) (key-pressed-p :right)))
          (accel-rate 10.0))

     ;; handle up and down movement
     (when up-p
       (with! player :accely accel-rate))
     (when down-p
       (with! player :accely (- accel-rate)))
     (when (and up-p down-p)
       (with! player :accely 0.0))
     (when (not (or up-p down-p))
       (cond ((< (abs (@ player :vely)) 2.0)
              (with! player :vely 0.0)
              (with! player :accely 0.0))
             (t (with! player :accely (* (- (signum (@ player :vely)))
                                         accel-rate 5.0)))))

     ;; left and right movement
     (when right-p
       (with! player :accelx accel-rate))
     (when left-p
       (with! player :accelx (- accel-rate)))
     (when (and left-p right-p)
       (with! player :accelx 0.0))
     (when (not (or right-p left-p))
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
         (with! *entities* id components))))

    (let ((player (@ *entities* *platformer-player*)))
      (with-slots (position) *camera*
        (setf position (vec3f (@ player :x) (@ player :y) 70.0))))

    ;; update view matrices based on camera
    (let ((cube-program (get-program "cube"))
          (rect-program (get-program "rect"))
          (view (get-view-matrix *camera*))
          (proj (kit.glm:perspective-matrix (kit.glm:deg-to-rad (zoom *camera*))
                                            (cfloat (/ *width* *height*))
                                            0.1 1000.0)))
      (gl:use-program (id cube-program))
      (gl:uniform-matrix-4fv (get-uniform cube-program "view") view nil)
      (gl:uniform-matrix-4fv (get-uniform cube-program "projection") proj nil)

      (gl:use-program (id rect-program))
      (gl:uniform-matrix-4fv (get-uniform rect-program "view") view nil)
      (gl:uniform-matrix-4fv (get-uniform rect-program "projection") proj nil))))


(defun platformer-render-entities ()
  (do-map (id components *entities*)
    (declare (ignore id))
    (rect-draw :position (vec3f (@ components :x) (@ components :y) (@ components :z))
               :size (vec2f (@ components :w) (@ components :h))
               :color (@ components :color)
               :draw-center (vec3f -0.5 0.5 0.0))))
(defun platformer-render-level ()
  (do-seq (components *platformer-level*)
    (rect-draw :position (vec3f (@ components :x) (@ components :y) (@ components :z))
               :size (vec2f (@ components :w) (@ components :h))
               :draw-center (vec3f -0.5 0.5 0.0))))

(let ((render-timer (make-timer :end (/ 1.0 60.0))))
  (defun platformer-render ()
    (timer-update render-timer)
    (when (timer-ended-p render-timer)

      (gl:enable :blend :depth-test)
      ;; (gl:disable :cull-face)
      (gl:blend-func :src-alpha :one-minus-src-alpha)
      (gl:clear-color 0.0 0.0 0.0 1.0)
      (gl:clear :color-buffer-bit :depth-buffer-bit)

      (platformer-render-level)
      (platformer-render-entities)

      (let* ((player (@ *entities* *platformer-player*))
             (text-scale (vec2f 1.0 1.0))
             (player-data-text
               (format nil "x:~6f,  y:~6f, vx:~6f, vy:~6f, ax:~6f, ay:~6f"
                       (@ player :x) (@ player :y)
                       (@ player :velx) (@ player :vely)
                       (@ player :accelx) (@ player :accely))))
        ;; (cube-draw :position (vec3f (@ player :x) (@ player :y) 0.0)
        ;;            :size (vec3f 1.0 1.0 1.0)
        ;;            :color (vec4f 0.0 0.0 1.0 1.0)
        ;;            :draw-center (vec3f -0.5 0.5 0.0))
        ;; (cube-draw :position (vec3f 0.0 10.0 0.0)
        ;;            :size (vec3f 1.0 1.0 1.0))
        ;; (cube-draw :position (vec3f 10.0 0.0 0.0))
        (multiple-value-bind (w h) (text-dimensions player-data-text
                                                    (get-font "sans24")
                                                    :scale text-scale)
          (text-draw player-data-text
                     (get-font "sans24")
                     :draw-center (vec3f -0.5 -0.5 0.0)
                     :position (vec3f (cfloat (- *width* w)) (cfloat (- *height* h)) 0.0)
                     :scale text-scale)))

      ;; fps
      (text-draw (format nil "~4f" (cfloat (average-fps)))
                 (get-font "sans24")
                 :position (vec3f 0.0 0.0 0.0)
                 :scale (vec2f 1.0 1.0)
                 :draw-center (vec3f -0.5 -0.5 0.0))

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
