;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :err)

(defmacro add-event (&key code)
  `(alexandria:appendf *destructive-changes* (list (lambda () ,code))))

(defun update-events ()
  (mapcar #'funcall *destructive-changes*)
  (setf *destructive-changes* nil)
  t)
