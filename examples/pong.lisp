(in-package :err-examples)

(defenum:defenum *enum-pong-state* ((+pong-menu+ 0)
                                    +pong-game+
                                    +pong-paused+
                                    +pong-game-over+))
(defglobal *pong-state* +pong-game+)

(defglobal *pong-score* 0)

(defglobal *pong-paddle-top* nil)
(defglobal *pong-paddle-left* nil)
(defglobal *pong-paddle-right* nil)
(defglobal *pong-paddle-bot* nil)

(defglobal *pong-ball* nil)

(defglobal *pong-walls* (empty-seq))

(defglobal *pong-screen-width* 100.0)
(defglobal *pong-screen-height 100.0)

(defun make-rect (position size &key (color (vec4f 1.0 1.0 1.0 1.0)))
  (map (:position position)
       (:size size)
       (:color color)
       (:collision-type 'wall)))

(defun make-movable-rect (position size
                          &key
                            (color (vec4f 1.0 1.0 1.0 1.0))
                            (collision-type 'paddle))
  (-> (make-rect position size :color color)
      (with :velocity (vec2f 0.0 0.0))
      (with :max-velocity (vec2f 50.0 50.0))
      (with :acceleration (vec2f 0.0 0.0))
      (with :collision-type collision-type)))

(defun make-border (position total-size border-size
                    &key (color (vec4f 1.0 1.0 1.0 1.0)))
  (seq
   ;; left
   (make-rect position
              (vec2f (y-val border-size) (x-val border-size))
              :color color)
   ;;right
   (make-rect (vec3f (- (+ (x-val position) (x-val total-size)) (y-val border-size))
                     (y-val position)
                     (z-val position))
              (vec2f (y-val border-size) (x-val border-size))
              :color color)
   ;;top
   (make-rect position
              border-size
              :color color)
   ;;bot
   (make-rect (vec3f (x-val position)
                     (+ (- (y-val position) (y-val total-size)) (y-val border-size))
                     (z-val position))
              border-size
              :color color)))

(defun pong-game-init ()
  (let* ((l1 30.0) ;; length 1
         (l2 3.0) ;; length 2
         (gap l2)
         (screen-w *pong-screen-width*)
         (screen-h *pong-screen-height))

    ;; paddle starting positions
    (setf *pong-paddle-top* (add-entity (make-movable-rect
                                         (vec3f (- (/ screen-w 2.0) (/ l1 2.0))
                                                (- screen-h gap)
                                                0.0)
                                         (vec2f l1 l2)))
          *pong-paddle-bot* (add-entity (make-movable-rect
                                         (vec3f (- (/ screen-w 2.0) (/ l1 2.0))
                                                (+ l2 gap)
                                                0.0)
                                         (vec2f l1 l2)))
          *pong-paddle-left* (add-entity (make-movable-rect
                                          (vec3f gap
                                                 (+ (/ screen-h 2.0) (/ l1 2.0))
                                                 0.0)
                                          (vec2f l2 l1)))
          *pong-paddle-right* (add-entity (make-movable-rect
                                           (vec3f (- screen-w gap l2)
                                                  (+ (/ screen-h 2.0) (/ l1 2.0))
                                                  0.0)
                                           (vec2f l2 l1)))
          *pong-ball* (add-entity (-> (make-movable-rect
                                       (vec3f (- (/ screen-w 2.0) (/ l2 2.0))
                                              (+ (/ screen-h 2.0) (/ l2 2.0))
                                              0.0)
                                       (vec2f l2 l2)
                                       :collision-type 'ball)
                                      (with :max-velocity (vec2f 100.0 100.0))
                                      (with :moving-p nil))))
    (setf *pong-walls* (make-border (vec3f (- gap) (+ screen-h gap) 0.0)
                                    (vec2f (+ screen-w (* 2 gap)) (+ screen-h (* 2 gap)))
                                    (vec2f (+ screen-w (* 2 gap)) gap)
                                    :color (vec4f 0.1 0.1 0.5 1.0)))))

(defun pong-init ()
  (initialize-globals)
  (init-shaders)

  (pong-game-init))


(defun pong-input ()
  (when (key-action-p :escape :press)
    (glfw:set-window-should-close))
  (when (and (key-pressed-p :left-control)
             (key-pressed-p :left-alt)
             (key-action-p :r :press))
    (pong-init))

  (cond ((equalp *pong-state* +pong-game+)
         (let* ((left (@ *entities* *pong-paddle-left*))
                (right (@ *entities* *pong-paddle-right*))
                (top (@ *entities* *pong-paddle-top*))
                (bot (@ *entities* *pong-paddle-bot*))
                (up-p (or (key-pressed-p :w) (key-pressed-p :up)))
                (down-p (or (key-pressed-p :s) (key-pressed-p :down)))
                (left-p (or (key-pressed-p :a) (key-pressed-p :left)))
                (right-p (or (key-pressed-p :d) (key-pressed-p :right)))
                (left-accel (@ left :acceleration))
                (left-vel (@ left :velocity))
                (top-accel (@ top :acceleration))
                (top-vel (@ top :velocity))
                (accel-rate 10.0))
           (when up-p
             (setf (y-val left-accel) accel-rate))
           (when down-p
             (setf (y-val left-accel) (- accel-rate)))
           (when (not (or up-p down-p))
             (cond ((< (abs (y-val left-vel)) 2.0)
                    (setf left-accel (vec2f 0.0 0.0)
                          left-vel (vec2f 0.0 0.0)))
                   (t (setf (y-val left-accel) (* (- (signum (y-val left-vel)))
                                                  accel-rate 5.0)))))

           (when left-p
             (setf (x-val top-accel) (- accel-rate)))
           (when right-p
             (setf (x-val top-accel) accel-rate))
           (when (not (or left-p right-p))
             (cond ((< (abs (x-val top-vel)) 2.0)
                    (setf top-accel (vec2f 0.0 0.0)
                          top-vel (vec2f 0.0 0.0)))
                   (t (setf (x-val top-accel) (* (- (signum (x-val top-vel)))
                                                 accel-rate 5.0)))))

           (setf left (-> left
                          (with :acceleration left-accel)
                          (with :velocity left-vel))
                 right (-> right
                           (with :acceleration left-accel)
                           (with :velocity left-vel))
                 top (-> top
                         (with :acceleration top-accel)
                         (with :velocity top-vel))
                 bot (-> bot
                         (with :acceleration top-accel)
                         (with :velocity top-vel)))

           (setf *entities* (-> *entities*
                                (with *pong-paddle-bot* bot)
                                (with *pong-paddle-left* left)
                                (with *pong-paddle-right* right)
                                (with *pong-paddle-top* top)))))))

(defun pong-render-game ()
  (with-slots (position) *camera*
    (setf position (vec3f 33.0 50.0 130.0))
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

  (do-seq (comps *pong-walls*)
    (rect-draw :position (@ comps :position)
               :size (@ comps :size)
               :color (@ comps :color)
               :draw-center (vec3f -0.5 0.5 0.0)))

  (do-map (id comps *entities*)
    (declare (ignore id))
    (rect-draw :position (@ comps :position)
               :size (@ comps :size)
               :color (@ comps :color)
               :draw-center (vec3f -0.5 0.5 0.0)))

  ;; show score
  (text-draw (format nil "~a" *pong-score*)
             (get-font "sans24")
             :position (vec3f 50.0 (cfloat (/ *height* 2.0)) 0.0)
             :scale (vec2f 1.5 1.5)))

(let ((render-timer (make-timer :end (/ 1.0 60.0))))
  (defun pong-render ()
    (timer-update render-timer)
    (when (timer-ended-p render-timer)
      (gl:enable :blend :depth-test)
      (gl:blend-func :src-alpha :one-minus-src-alpha)
      (gl:clear-color 0.0 0.0 0.0 1.0)
      (gl:clear :color-buffer-bit :depth-buffer-bit)

      (cond ((equalp *pong-state* +pong-game+)
             (pong-render-game)
             (text-draw (format nil "~a ~a"
                                (y-val (@ (@ *entities* *pong-paddle-left*)
                                          :velocity))
                                (y-val (@ (@ *entities* *pong-paddle-left*)
                                          :acceleration)))
                        (get-font "sans24")
                        :position (vec2f 100.0 50.0))))

      ;; fps
      (text-draw (format nil "~4f" (average-fps))
                 (get-font "sans24")
                 :position (vec3f 0.0 0.0 0.0)
                 :scale (vec2f 0.5 0.5)
                 :draw-center (vec3f -0.5 -0.5 0.0))

      (timer-reset render-timer))))

(defun pong-valid-move-p (position size entity-id)
  "=> BOOLEAN, OBJECT, ID"
  (let ((valid-p t)
        (obj)
        (collide-id))
    (block col-loop
      (do-seq (wall *pong-walls*)
        (let* ((pos (@ wall :position))
               (wall-size (@ wall :size))
               (x (x-val pos))
               (y (y-val pos))
               (w (x-val wall-size))
               (h (y-val wall-size)))
          (when (rects-collide-p (x-val position) (y-val position)
                                 (x-val size) (y-val size)
                                 x y w h)
            (setf valid-p nil
                  obj wall)
            (return-from col-loop))))

      (do-map (id comps *entities*)
        (let* ((pos (@ comps :position))
               (ent-size (@ comps :size))
               (x (x-val pos))
               (y (y-val pos))
               (w (x-val ent-size))
               (h (y-val ent-size)))
          (unless (eql id entity-id)
            (when (rects-collide-p (x-val position) (y-val position)
                                   (x-val size) (y-val size)
                                   x y w h)
              (setf valid-p nil
                    obj comps
                    collide-id id)
              (return-from col-loop))))))
    (values valid-p obj collide-id)))

(defun pong-update-game (dt)
  (do-map (id comps *entities*)
    (let* ((pos (@ comps :position))
           (size (@ comps :size))
           (vel (@ comps :velocity))
           (max-vel (@ comps :max-velocity))
           (accel (@ comps :acceleration))
           (accel/2 (vec2f* accel (* 0.5 dt)))
           (col-type (@ comps :collision-type))
           (color (@ comps :color)))
      (setf
       ;; add half acceleration
       vel (vec2f+ vel accel/2)

       ;; cap velocity
       vel (vec2f-clamp vel (vec2f* max-vel -1.0) max-vel))

      ;; add velocity to position
      ;; x pass
      (let ((move-x (vec3f+ pos (vec3f (* (x-val vel) dt) 0.0 0.0))))
        (multiple-value-bind (valid-move-p object object-id)
            (pong-valid-move-p move-x size id)
          (cond (valid-move-p
                 (setf color (vec4f 0.2 0.5 0.6 0.6))
                 (setf pos move-x))
                (t
                 (setf color (vec4f 0.5 0.1 0.1 1.0))
                 (let ((obj-col-type (@ object :collision-type)))
                   (cond
                     ;;paddle-wall
                     ((and (equalp col-type 'paddle) (equalp obj-col-type 'wall))
                      (setf vel (vec2f 0.0 0.0)
                            accel (vec2f 0.0 0.0)
                            accel/2 (vec2f 0.0 0.0)
                            ;; color (vec4f 1.0 0.0 0.0 0.5)
                            ))
                     ;;paddle-ball
                     ;;ball-wall
                     (t
                      (setf pos move-x))))))))
      ;; y pass
      (let ((move-y (vec3f+ pos (vec3f 0.0 (* (y-val vel) dt) 0.0))))
        (cond ((pong-valid-move-p move-y size id)
               (setf pos move-y))))

      (setf
       ;; more acceleration
       vel (vec2f+ vel accel/2)

       ;; make update to the locally scoped components
       comps (-> comps
                 (with :color color)
                 (with :accel accel)
                 (with :position pos)
                 (with :velocity vel))))

    ;; update entities

    (add-event
     :code
     (with! *entities* id comps))))

(let ((update-timer (make-timer :end (/ 1.0 100.0))))
  (defun pong-update ()
    (timer-update update-timer)
    (iter (while (timer-ended-p update-timer))
      (timer-keep-overflow update-timer)
      (when (equalp *pong-state* +pong-game+)
        (pong-update-game (timer-end update-timer))))))

(defun pong-cleanup ())

(defun pong ()
  (err:run "pong"
           :init-code (pong-init)
           :input-code (pong-input)
           :render-code (pong-render)
           :update-code (pong-update)
           :cleanup-code (pong-cleanup)))
