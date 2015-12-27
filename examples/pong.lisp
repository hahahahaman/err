(in-package :err-examples)

(defun pong-init ())
(defun pong-input ())
(defun pong-render ())
(defun pong-update ())
(defun pong-cleanup ())

(defun pong ()
  (err:run "pong"
           :init-code (pong-init)
           :input-code (pong-input)
           :render-code (pong-render)
           :update-code (pong-update)
           :cleanup-code (pong-cleanup)))
