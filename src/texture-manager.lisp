(in-package :err)

(defclass texture-manager (resource-manager)
  ())

(defun load-texture (name resource &optional (manager *texture-manager*))
  (let ((texture (@ (resources manager) name)))
    (when texture
      (gl:delete-textures (list (id texture)))))
  (load-resource name resource manager))

(defun get-texture (name &optional (manager *texture-manager*))
  (get-resource name manager))

(defmethod clear-resources ((manager texture-manager))
  (gl:delete-textures (convert 'list (image (lambda (name resource)
                                              (declare (ignore name))
                                              resource)
                                            (resources manager)))))
