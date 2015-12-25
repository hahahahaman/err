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
(defglobal *height* 600)

(defglobal *debug* nil)

;;; delta time
(defconstant +max-fps+ 150)
(defglobal *dt* 0.02d0) ;; *DT* keeps track of the time since last frame, in seconds
(defglobal *previous-time* 0.0) ;; *PREVIOUS-TIME* gives the time of the previous frame

#|
input
|#

;; p-lists that keep track of the current actions on keys and buttons
(defglobal *key-actions* ())
(defglobal *mouse-button-actions* ())
(defglobal *key-pressed* ())
(defglobal *mouse-button-pressed* ())

;;; cursor position values
(defglobal *cursor-callback-p* nil) ;; cursor has been moved
(defglobal *first-mouse* t) ;; checks if first time cursor has been moved

;; current cursor position
(defglobal *cursor-x* (/ *width* 2.0))
(defglobal *cursor-y* (/ *height* 2.0))

;; previous cursor position
(defglobal *last-x* (/ *width* 2.0))
(defglobal *last-y* (/ *height* 2.0))

;; the scroll wheel has been used
(defglobal *scroll-callback-p* nil)

;; number of ticks of the scroll wheel
(defglobal *scroll-x* (/ *width* 2.0))
(defglobal *scroll-y* (/ *height* 2.0))

#|
time-travel
|#

(defenum:defenum *enum-time-travel-state* ((+time-play+ 0)
                                           +time-paused+
                                           +time-rewind+
                                           +time-forward+))

(defglobal *time-travel-state* +time-play+)
(defglobal *current-frame* 0)
(defglobal *max-frame-index* 0)
(defglobal *timeline*
    (make-array 500000 :element-type 'list
                       :initial-element nil
                       :adjustable t
                       :fill-pointer 0))

(defglobal *tracked-vars* nil)

;;; rewind and fast-forward
(defglobal *time-speed-multiplier* (vector 1 2 4 8 16 32))
(defglobal *time-speed-index* 0)

#|
events
|#

(defglobal *destructive-changes* ())

#|
resource-manager
|#

(defglobal *program-manager* nil)
(defglobal *texture-manager* nil)
(defglobal *font-manager* nil)

#|
camera
|#

(defglobal *camera*)

#|
drawer
|#

(defglobal *sprite-drawer* nil)
(defglobal *rect-drawer* nil)
(defglobal *text-drawer* nil)
(defglobal *cube-drawer* nil)

#|
entity
|#

(defglobal *entities* (empty-map))

#|
file tracking
|#
;; filepath : (checksum . hook)
(defglobal *tracked-files* (make-hash-table :test 'equal))

#|
for when creating an executable
|#
(defglobal *executable* nil)

#|
keep track of vec types that have been defined
|#
(defparameter *vec-types* '())
