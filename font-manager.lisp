(in-package :err)

(defclass font-manager (resource-manager)
  ())

(defun load-font (name resource &optional (pixel-size 14) (manager *font-manager*))
  (ft2:set-pixel-sizes resource 0 pixel-size)
  (load-resource name resource manager))

(defun get-font (name &optional (manager *font-manager*))
  (get-resource name manager))
