(in-package :err-examples)

(defun platformer-init ())

(defun platformer-handle-input ())

(defun platformer-update ())

(defun platformer-render ())

(defun platformer-cleanup ())

(defmacro platfomer-start ()
  `(err:run "platformer"
            :init-code (platformer-init)
            :input-code (platformer-handle-input)
            :render-code (platformer-render)
            :update-code (platformer-update)
            :cleanup-code (platformer-cleanup)))

(defun platformer ()
  )
