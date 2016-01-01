(in-package :err-examples)

(defglobal *platformer-player* nil)
(defglobal *platformer-level* (empty-seq))

(defun rects-collide-p (x y w h ox oy ow oh)
  "Rectangle collision check based on bottom left position of the rectangle."
  (not (or (< (+ ox ow) x)
           (> ox (+ x w))
           (< oy (- y h))
           (> (- oy oh) y))))

(defun platformer-init ()
  (init-shaders)

  ;; player
  (setf *platformer-player* (map (:x 32.0)
                                 (:y 32.0)
                                 (:w 32)
                                 (:h 32)
                                 (:velx 0.0)
                                 (:vely 0.0)
                                 (:accelx 0.0)
                                 (:accely 0.0)))
  ;;platforms
  (setf *platformer-level* (with-last *platformer-level* (map (:x 0.0)
                                                              (:y 0.0)
                                                              (:w 100.0)
                                                              (:h 100.0)))))

(defun platformer-handle-input ()
  (when (key-action-p :escape :press)
    (glfw:set-window-should-close)))

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
                (accelx (@ components :accelx))
                (accely (@ components :accely))
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
                     (with :y y)))))))))

(let ((render-timer (make-timer :end (/ 1.0 60.0))))
  (defun platformer-render ()
    (timer-update render-timer)
    (when (timer-ended-p render-timer)

      (gl:enable :blend :depth-test)
      ;; (gl:disable :cull-face)
      (gl:blend-func :src-alpha :one-minus-src-alpha)
      (gl:clear-color 0.0 0.0 0.0 1.0)
      (gl:clear :color-buffer-bit :depth-buffer-bit)

      (cube-draw :position (vec3f 0.0 0.0 0.0))

      ;; fps
      (text-draw (format nil "~4f" (cfloat (average-fps)))
                 (get-font "sans24")
                 :position (vec2 1.0 3.0)
                 :scale (vec2 0.7 0.7))
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
