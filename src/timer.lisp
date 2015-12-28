(in-package :err)

(defstruct timer
  (end 1.0 :type real)
  (time 0.0 :type real))

(defmethod timer-ended-p ((timer timer))
  (>= (timer-time timer) (timer-end timer)))

(defmethod timer-update ((timer timer))
  (incf (timer-time timer) *dt*))

(defmethod timer-reset ((timer timer))
  (setf (timer-time timer) 0.0))

(defmethod timer-keep-overflow ((timer timer))
  (decf (timer-time timer) (timer-end timer)))
