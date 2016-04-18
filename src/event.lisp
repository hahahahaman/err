;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :err)

(defmacro add-event (&key code (priority 1))
  "A macro which adds a new lambda function which executes CODE. The lambda is
added to *DESTRUCTIVE-CHANGES* in the given PRIORITY. Lower meaning more
priority, and with only 3 levels of actual priority ranging from 0 to
2 (inclusive)."
  (cond ((> priority 2) (setf priority 2))
        ((< priority 0) (setf priority 0)))
  `(vector-push-extend (lambda () ,code) (aref *destructive-changes* ,priority)))

(defmacro defevent (name parameters &body body)
  "Creates a function that calls ADD-EVENT."
  `(defun ,name ,parameters
     (add-event :code (progn ,@body))))

(defun update-events ()
  "Iterates through all the added events in order of the priority of the events.
There are 3 levels of priority: 0 - before, 1 - current, and 2 - after."
  (iter (for i from 0 below 3) ;; loop through all changes in order of priority
    (let* ((changes (aref *destructive-changes* i))
           (counter 0)
           (len (length changes)))

      ;; keeps looping through changes, checking for new changes
      (iter (while (> len counter))

        ;; loops through all current changes, which can add more changes
        (iter (while (< counter len))
          (funcall (aref changes counter))
          (incf counter))

        ;; changes the length to see if it has increased
        (setf len (length changes)))

      ;; reinitialize the array removing all changes
      (setf *destructive-changes*
            (make-array 3 :element-type 'vector
                          :initial-element
                          (make-array 0 :adjustable t
                                        :fill-pointer 0
                                        :element-type 'function)))))
  t)
