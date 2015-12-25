(in-package :err)

(defun track-file (path hook)
  (setf (gethash path *tracked-files*)
        (cons (md5 (alexandria:read-file-into-string path))
              hook)))

(defun untrack-file (path)
  (remhash path *tracked-files*))

(defun verify-files ()
  (iter (for (path data) in-hashtable *tracked-files*)
    (let ((checksum (car data))
          (hook (cdr data))
          (file (alexandria:read-file-into-string path)))
      ;; file changed
      (when (not (valid-checksum-p checksum (md5 file)))
        (funcall hook)))))

(let ((update-timer (make-timer :end 0.5)))
  (defun update-files ()
    (timer-update update-timer)
    (when (timer-ended-p update-timer)
      (verify-files)
      (timer-reset update-timer))))
