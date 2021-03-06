(in-package :err-examples)

(defglobal *platformer-player* nil)
(defglobal *platformer-level* (empty-seq))
(defglobal *platformer-gravity* 10.0)

;; (defun move-entity (entity)
;;   "=> ENTITY (MAP)
;; Moves the entity, checking collision along the way."
;;   )

(defun platformer-init ()
  (initialize-globals)
  (init-shaders)

  ;; player
  (let* ((proj-dir (asdf:system-source-directory :err-examples))
         (images-dir (if *executable*
                         #p"./data/images/"
                         (merge-pathnames #p"examples/data/images/" proj-dir)) ))
    (setf *platformer-player*
          (add-entity (map (:x 10.0)
                           (:y 10.0)
                           (:z 0.0)
                           (:w 5.0)
                           (:h 5.0)
                           (:color (vec4f 1.0 1.0 1.0 1.0))
                           (:texture (make-texture2d (namestring (merge-pathnames
                                                                  #p"yoshi.png"
                                                                  images-dir))))
                           (:velx 0.0)
                           (:vely 0.0)
                           (:accelx 0.0)
                           (:accely 0.0)
                           (:max-velx 50.0)
                           (:max-vely 100.0)
                           (:gravity-p t)
                           (:jump-p nil)
                           (:face-right-p t)))))
  ;;platforms
  (setf *platformer-level* (-> *platformer-level*
                               ;; bottom
                               (with-last (map (:x 0.0)
                                               (:y 0.0)
                                               (:z 0.0)
                                               (:w 1000.0)
                                               (:h 5.0)))

                               ;; left
                               (with-last (map (:x 0.0)
                                               (:y 100.0)
                                               (:z 0.0)
                                               (:w 5.0)
                                               (:h 100.0)))

                               ;; top
                               (with-last (map (:x 0.0)
                                               (:y 100.0)
                                               (:z 0.0)
                                               (:w 100.0)
                                               (:h 5.0)))

                               ;; right
                               ;; (with-last (map (:x 95.0)
                               ;;                 (:y 95.0)
                               ;;                 (:z 0.0)
                               ;;                 (:w 5.0)
                               ;;                 (:h 100.0)))
                               )))

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
          (accel-rate 10.0)
          (jump-p (@ player :jump-p)))

     ;; handle up and down movement
     (when up-p
       (when jump-p
         (includef player :vely 30.0)
         (includef player :jump-p nil))
       ;; (includef player :accely accel-rate)
       )
     (when down-p
       (includef player :accely (- accel-rate)))
     ;; (when (and up-p down-p)
     ;;   (includef player :accely 0.0))
     ;; (when (not (or up-p down-p))
     ;;   (cond ((< (abs (@ player :vely)) 2.0)
     ;;          (includef player :vely 0.0)
     ;;          (includef player :accely 0.0))
     ;;         (t (includef player :accely (* (- (signum (@ player :vely)))
     ;;                                     accel-rate 5.0)))))

     ;; left and right movement
     (when right-p
       (includef player :accelx accel-rate))
     (when left-p
       (includef player :accelx (- accel-rate)))
     (when (and left-p right-p)
       (includef player :accelx 0.0))
     (when (not (or right-p left-p))
       (cond ((< (abs (@ player :velx)) 2.0)
              (includef player :velx 0.0)
              (includef player :accelx 0.0))
             (t (includef player :accelx (* (- (signum (@ player :velx)))
                                         accel-rate 5.0)))))

     (includef *entities* *platformer-player* player))))

(defun platformer-valid-move-p (x y w h)
  (let ((result t))
    (do-seq (components *platformer-level*)
      (when (rects-collide-p x y w h
                             (@ components :x) (@ components :y)
                             (@ components :w) (@ components :h))
        (setf result nil)))
    result))

(defupdate platformer-update 200.0
  (add-event
   :code
   (progn
     (do-map (id components *entities*)
       (let* ((x (@ components :x))
              (y (@ components :y))
              (w (@ components :w))
              (h (@ components :h))
              (velx (@ components :velx))
              (vely (@ components :vely))
              (max-velx (@ components :max-velx))
              (max-vely (@ components :max-vely))
              (gravity-p (@ components :gravity-p))
              (accelx (@ components :accelx))
              (accely (@ components :accely))
              ;; (max-accelx (@ components :max-accelx))
              ;; (max-accely (@ components :max-accely))
              (dt (timer-end update-timer)) ;; timestep
              (accelx/2 (* accelx 0.5 dt))
              (accely/2 (* (- accely (if gravity-p 50.0 0.0)) 0.5 dt))
              (face-right-p (@ components :face-right-p)))

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

         (cond ((platformer-valid-move-p (+ x (* velx dt)) y w h)
                (incf x (* velx dt)))
               (t (setf velx 0.0
                        accelx 0.0
                        accelx/2 0.0)))

         (cond  ((platformer-valid-move-p x (+ y (* vely dt)) w h)
                 (incf y (* vely dt)))
                (t (setf vely 0.0
                         accely 0.0
                         accely/2 0.0)
                   (includef components :jump-p t)))

         (incf velx accelx/2)
         (incf vely accely/2)

         (cond ((> velx 0.0) (setf face-right-p t))
               ((< velx 0.0) (setf face-right-p nil)))

         ;; update values
         (setf components
               (-> components
                   (with :accelx accelx)
                   (with :accely accely)
                   (with :velx velx)
                   (with :vely vely)
                   (with :x x)
                   (with :y y)
                   (with :face-right-p face-right-p))))

       ;; update the entity
       (includef *entities* id components))))

  ;; update camera
  (add-event
   :priority 2
   :code
   (progn (let* ((player (@ *entities* *platformer-player*))
                 (x (@ player :x))
                 (y (@ player :y))
                 (w (@ player :w))
                 (h (@ player :h))
                 (velx (@ player :velx))
                 (vely (@ player :vely))
                 (dt (timer-end update-timer)))
            (with-slots (position) *camera*

              ;; static
              ;; (setf position (vec3f 50.0 0.0 100.0))

              ;; focus player
              ;; (setf position (vec3f (@ player :x) (@ player :y) 100.0))

              ;; adjust camera based on velocity of player
              ;; (incf (x-val position) (* velx dt))
              ;; (incf (y-val position) (* vely dt))

              ;; focus a position ahead of the player
              (let* ((x-forwardness velx)
                     (y-forwardness (* vely 1.3))
                     (x-diff (+ (- (+ x (/ w 2.0)) (x-val position))
                                x-forwardness))
                     (y-diff (+ (- (- y (/ h 2.0)) (y-val position))))
                     (change 1.5))
                (incf (x-val position) (* change x-diff dt))
                (incf (y-val position) (* change y-diff dt)))
              (setf (z-val position) 100.0)
              (update-camera-vectors *camera*)))

          ;; gotta do this in sequence with the camera position change
          ;; or else the movement will look choppy
          ;; update view matrices based on camera
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
            (gl:uniform-matrix-4fv (get-uniform sprite-program "projection") proj nil))
          )))

(defun platformer-render-entities ()
  (do-map (id components *entities*)
    (declare (ignore id))
    (let ((x (@ components :x))
          (y (@ components :y))
          (z (@ components :z))
          (w (@ components :w))
          (h (@ components :h))
          (color (@ components :color))
          (face-right-p (@ components :face-right-p)))
      (rect-draw :position (vec3f x y z)
                 :size (vec2f w h)
                 :color color
                 :draw-center (vec3f -0.5 0.5 0.5))
      (sprite-draw (@ components :texture)
                   :position (vec3f x y z)
                   :size (vec2f w h)
                   :color color
                   :rotation (vec3f 0.0 (if face-right-p (cfloat pi) 0.0) 0.0)
                   :draw-center (vec3f -0.5 0.5 0.0)
                   :clip-position (vec2f 11.0 128.0)
                   :clip-size (vec2f 45.0 62.0))
      )))

(defun platformer-render-level ()
  (do-seq (components *platformer-level*)
    (rect-draw :position (vec3f (@ components :x) (@ components :y) (@ components :z))
               :size (vec2f (@ components :w) (@ components :h))
               :color (vec4f 0.0 0.5 0.2 1.0)
               :draw-center (vec3f -0.5 0.5 0.0))))

(defrender platformer-render 200.0
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
                   (@ player :accelx) (@ player :accely)))
         ;; trying to figure out if object is in camera view
         ;; (object-in-view (kit.glm:matrix*vec4
         ;;                  (kit.glm:matrix* (get-view-matrix *camera*)
         ;;                                   (kit.glm:translate* 0.0 0.0 -1.0))
         ;;                  (vec4f 0.0 0.0 -1.0 1.0)))
         )
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
                 :scale text-scale))

    ;; (text-draw (format nil "~a ~a ~a"
    ;;                    (x-val object-in-view)
    ;;                    (y-val object-in-view)
    ;;                    (z-val object-in-view))
    ;;            (get-font "sans24")
    ;;            :position (vec2f 0.0 100.0)
    ;;            :draw-center (vec3f -0.5 -0.5 0.0))
    )

  ;; fps
  (text-draw (format nil "~4f" (cfloat (average-fps)))
             (get-font "sans24")
             :position (vec3f 0.0 0.0 0.0)
             :scale (vec2f 0.5 0.5)
             :draw-center (vec3f -0.5 -0.5 0.0)))

(defun platformer-cleanup ())

(defmacro platformer-start ()
  `(err-run "platformer"
            :init-code (platformer-init)
            :input-code (platformer-handle-input)
            :render-code (platformer-render)
            :update-code (platformer-update)
            :cleanup-code (platformer-cleanup)))

(defun platformer ()
  (platformer-start))
