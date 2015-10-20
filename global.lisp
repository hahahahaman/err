(in-package #:err)

(defvar *global-setfs* nil)

(defmacro defglobal (var &optional (val nil valp) (doc nil docp))
  "Creates a global variable and adds a lambda to *GLOBAL-SETFS* that
can be used to reset VAR to VAL."
  (append `(progn)
          (if (and valp docp)
              `((defvar ,var ,val ,doc))
              `((defvar ,var ,val)))
          `((setf (getf *global-setfs* (intern (string ',var) :keyword))
                  (lambda () (setf ,var ,val))))))

;;;; misc. globals
;;; screen
(defglobal *width* 800)
(defglobal *height* 800)

;;; delta time
;; *DT* keeps track of the time since last frame, in seconds
;; *PREVIOUS-TIME* gives the time, in seconds, of the previous frame
;; since the start of the program, and (glfw:get-time) returns the
;; current time
(defconstant +max-fps+ 150)
(defglobal *dt* 0.02d0)
(defglobal *previous-time* 0.0)
