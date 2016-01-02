(in-package :err)

(let ((id 0))
  (defun make-entity (components &optional (entities *entities*))
    "=> MAP, ID
Returns the a fset map of ENTITIES, with a new key-value pair of the
new id and COMPONENTS. The second value is the id."
    (incf id)
    (values (with entities id components) id)))

(defun add-entity (components &optional (entities *entities*))
  "=> ID
Destructively updates the global *ENTITIES* with a new entity."
  (multiple-value-bind (ents id) (make-entity components entities)
    (setf *entities* ents)
    id))

;; (defun add-entities (entity-list &optional (entities *entities*))
;;   (append entities entity-list))

(defun remove-entity (id &optional (entities *entities*))
  (less entities id))
(defmacro remove-entities (ids &optional (entities *entities*))
  `(-> ,entities ,@(mapcar (lambda (id) `(less ,id)) ids)))

(defun get-component (component id &optional (entities *entities*))
  (@ (@ entities id) component))
(defun set-component  (component id value &optional (entities *entities*))
  (with entities id (with (@ entities id) component value)))

;; (defmacro set-component (component entity-id value)
;;   `(setf ,components (plist-set ,components ,component ,value)))
;; (defun (setf get-component) (value component entity-id &optional (entities *entities*))
;;   (setf (cdr (nth entity-id entities))))
;; ;; (defun (setf get-component) (value component components)
;; ;;   (setf components (plist-set components component value)))

(defun get-entity (id &optional (entities *entities*))
  (@ entities id))
(defun set-entity (id value  &optional (entities *entities*))
  (with entities id value))

(defun get-entity-component (component entity)
  (@ entity component))
(defun set-entity-component (component entity value)
  (with entity component value))

(defun find-entities (predicate &optional (entities *entities*))
  (get-map-keys 'list (filter predicate entities)))

(defun find-entity-by-component (components &optional (entities *entities*))
  (let ((found ()))
    (do-map (x y entities)
      ;; go through COMPONENTS to see if they are in the current entity
      ;; if one is not then return NIL, else return T
      (when (iter (for c in components)
              (when (null (nth-value 1 (@ y c))) (return nil))
              (finally (return t)))
        (push x found)))
    (reverse found)))
