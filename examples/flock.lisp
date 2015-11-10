;;; flocking behaviour

(defun flock-init ())
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
