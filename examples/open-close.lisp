(in-package :err-examples)

;; Basic example, opens then closes a window.

(defun oc-init ()
  (debug-print "open")
  (close-window))

(defun oc-input ())
(defun oc-render ())
(defun oc-update ())

(defun oc-cleanup ()
  (debug-print "close"))

(defun open-close ()
  (err-run "open-close"
           :init-code (oc-init)
           :input-code (oc-input)
           :render-code (oc-render)
           :update-code (oc-update)
           :cleanup-code (oc-cleanup)))
