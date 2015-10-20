;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :err)

(defglobal *destructive-changes* ())

(defun add-event (func)
  (alexandria:appendf *destructive-changes* (list func)))

(defun update-events ()
  (mapcar #'funcall *destructive-changes*)
  (setf *destructive-changes* nil)
  t)
