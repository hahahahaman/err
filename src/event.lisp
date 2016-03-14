;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :err)

(defmacro add-event (&key code)
  `(vector-push-extend (lambda () ,code) *destructive-changes*))

(defmacro defevent (name parameters &body body)
  `(defun ,name ,parameters
     (add-event :code (progn ,@body))))

(defun update-events ()
  (let ((counter 0)
        (len (length *destructive-changes*)))
    (iter (while (> len counter))
      (iter (while (< counter len))
        (funcall (aref *destructive-changes* counter))
        (incf counter))
      (setf len (length *destructive-changes*)))
    (setf *destructive-changes* (make-array 0 :adjustable t :fill-pointer 0 :element-type 'function)))
  t)
