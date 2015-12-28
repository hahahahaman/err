(in-package :err-examples)

(defglobal *platformer-level* (empty-seq))

(defun rect-collide-p (x y w h ox oy ow oh)
  "Rectangle collision check based on bottom left position of the rectangle."
  (not (or (< (+ ox ow) x)
           (> ox (+ x w))
           (< oy (- y h))
           (> (- oy oh) y))))

(defun platformer-init ()
  (init-shaders)

  ;;platforms
  (setf *platformer-level* (with-last *platformer-level* (map (:x 0.0)
                                                              (:y 0.0)
                                                              (:w 100)
                                                              (:h 100))))

  ;; player
  (add-entity (map (:x 32.0)
                   (:y 32.0)
                   (:w 32)
                   (:h 32)
                   (:velx 0.0)
                   (:vely 0.0)
                   (:accelx 0.0)
                   (:accely 0.0))))

(defun platformer-handle-input ()
  (when (key-action-p :escape :press)
    (glfw:set-window-should-close)))

(defun platformer-update ())

(defun platformer-render ())

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
