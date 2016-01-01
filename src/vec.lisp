(in-package :err)

;;;;;;;;;;;
;; vectors
;;;;;;;;;;;

;; (defgeneric vec-add (v1 v2)
;;   (:documentation
;;    "Returns a vector of the same type as V1, which is a component-wise sum of V1
;;    and V2.")
;;   (:method ((v1 vector) (v2 vector))
;;     (cl:map (type-of v1) #'+ v1 v2)))

;; (defgeneric vec-mul (v1 f)
;;   (:documentation
;;    "Returns a vector with the same type as V1, which has components multiplied
;;    by F.")
;;   (:method ((v1 vector) (f number))
;;     (cl:map (type-of v1)
;;             (lambda (x) (* (the single-float x) (the single-float f)))
;;             v1)))

;; (defgeneric vec-div (v1 f)
;;   (:documentation
;;    "Returns a vector with the same type as V1, which has components divided by
;;    F.")
;;   (:method ((v1 vector) (f number))
;;     (vec-mul v1 (/ 1.0 f))))

;; (defgeneric vec-length (v)
;;   (:documentation
;;    "Returns the square root of the sum of the components of V.")
;;   (:method ((v vector))
;;     (sqrt (reduce #'+ (cl:map (type-of v) #'(lambda (x) (expt x 2)) v)))))

;; (defgeneric clamp (value low high)
;;   (:documentation
;;    "Returns VALUE, such that it remains between LOW and HIGH.")
;;   (:method ((value number) (low number) (high number))
;;     (min high (max low value))))

;; (defgeneric vec-clamp (value low high)
;;   (:documentation
;;    "Returns the vector VALUE, such that its components remain between the
;;    components of LOW and HIGH.")
;;   (:method ((value vector) (low vector) (high vector))
;;     (cl:map (type-of value) #'clamp value low high)))

;; code from mathkit
(defmacro define-vecn (n type &optional (suffix ""))
  "Creates a new type of vector."
  (let* ((vecn (alexandria:symbolicate 'vec (format nil "~A" n)
                                       (string-upcase suffix)))
         (v+ (alexandria:symbolicate vecn "+"))
         (v- (alexandria:symbolicate vecn "-"))
         (v* (alexandria:symbolicate vecn "*"))
         (v/ (alexandria:symbolicate vecn "/"))
         (vadd (alexandria:symbolicate vecn "-ADD"))
         (vsub (alexandria:symbolicate vecn "-SUB"))
         (vmul (alexandria:symbolicate vecn "-MUL"))
         (vdiv (alexandria:symbolicate vecn "-DIV"))
         (vlength (alexandria:symbolicate vecn "-LENGTH"))
         (vdotproduct (alexandria:symbolicate vecn "-DOT-PRODUCT"))
         ;; (vcrossproduct (alexandria:symbolicate vecn "-CROSS-PRODUCT"))
         (vnormalize (alexandria:symbolicate vecn "-NORMALIZE"))
         (vclamp (alexandria:symbolicate vecn "-CLAMP"))
         (clamptype (alexandria:symbolicate 'clamp (string-upcase suffix)))
         (new-clamptype ;; clamp function only needs to be defined once per type
           (when (null (getf *vec-types* `(',type))) ;; check if type exists already
             (setf (getf *vec-types* `(',type)) 1)
             `(progn
                (declaim (ftype (function (,type ,type ,type) ,type) ,clamptype))
                (defun ,clamptype (value low high)
                  (declare (optimize (speed 3) (safety 0)))
                  (min high (the ,type (max low value))))))))

    (append
     new-clamptype
     `((progn
         (deftype ,vecn () '(simple-array ,type (,n)))
         (defun ,vecn (a &rest r)
           (etypecase a
             (vector
              (cond
                ((= (length a) ,n) a)
                ((> (length a) ,n)
                 (let ((a+ (make-array ,n :element-type ',type)))
                   (replace a+ a)
                   a+))
                (t (let* ((a+ (make-array ,n :element-type ',type)))
                     (replace a+ a)
                     (replace a+ r :start1 (length a))
                     a+))))
             (,type
              (let* ((a+ (make-array ,n :element-type ',type)))
                (setf (aref a+ 0) a)
                (replace a+ r :start1 1)
                a+))))

         (declaim (ftype (function (,vecn ,vecn) ,vecn) ,v+))
         (defun ,v+ (v1 v2)
           (declare (optimize (speed 3) (safety 0)))
           (,vecn ,@(iter (for i from 0 below n)
                      (collect `(+ (aref v1 ,i) (aref v2 ,i))))))

         (declaim (ftype (function (,vecn ,vecn) ,vecn) ,v-))
         (defun ,v- (v1 v2)
           (declare (optimize (speed 3) (safety 0)))
           (,vecn ,@(iter (for i from 0 below n)
                      (collect `(- (aref v1 ,i) (aref v2 ,i))))))

         (declaim (ftype (function (,vecn ,type) ,vecn) ,v*))
         (defun ,v* (v x)
           (declare (optimize (speed 3) (safety 0)))
           (,vecn ,@(iter (for i from 0 below n)
                      (collect `(* (aref v ,i) x)))))

         (declaim (ftype (function (,vecn ,type) ,vecn) ,v/))
         (defun ,v/ (v x)
           (declare (optimize (speed 3) (safety 0)))
           (,vecn ,@(iter (for i from 0 below n)
                      (collect `(/ (aref v ,i) x)))))

         (declaim (ftype (function (,vecn ,vecn) ,vecn) ,vadd))
         (defun ,vadd (v1 v2)
           (declare (optimize (speed 3) (safety 0)))
           (cl:map ',vecn #'+ v1 v2))

         (declaim (ftype (function (,vecn ,vecn) ,vecn) ,vsub))
         (defun ,vsub (v1 v2)
           (declare (optimize (speed 3) (safety 0)))
           (cl:map ',vecn #'- v1 v2))

         (declaim (ftype (function (,vecn ,type) ,vecn) ,vmul))
         (defun ,vmul (v n)
           (declare (optimize (speed 3) (safety 0)))
           (cl:map ',vecn (lambda (x)
                            (declare (,type x))
                            (the ,type (* x n))) v))

         (declaim (ftype (function (,vecn ,type) ,vecn) ,vdiv))
         (defun ,vdiv (v n)
           (declare (optimize (speed 3) (safety 0)))
           (,vmul v (/ 1 n)))

         (declaim (ftype (function (,vecn) float) ,vlength))
         (defun ,vlength (v)
           (declare (optimize (speed 3) (safety 0)))
           (sqrt (the ,type
                      (reduce #'+ (the ,vecn
                                       (cl:map ',vecn (lambda (x)
                                                        (declare (,type x))
                                                        (the ,type (* x x)))
                                               v))))))

         (declaim (ftype (function (,vecn ,vecn) float) ,vdotproduct))
         (defun ,vdotproduct (v1 v2)
           (declare (optimize (speed 3) (safety 0)))
           (+ ,@(iter (for i from 0 below n)
                  (collect `(* (aref v1 ,i) (aref v2 ,i))))))

         (declaim (ftype (function (,vecn) float) ,vdotproduct))
         (defun ,vnormalize (v)
           (,v/ v (,vlength v)))

         (declaim (ftype (function (,vecn ,vecn ,vecn) ,vecn) ,vclamp))
         (defun ,vclamp (value low high)
           (declare (optimize (speed 3) (safety 0)))
           (cl:map ',vecn (function ,clamptype) value low high)))))))

(define-vecn 2 fixnum "i")
(define-vecn 3 fixnum "i")
(define-vecn 4 fixnum "i")

(define-vecn 2 single-float "f")
(define-vecn 3 single-float "f")
(define-vecn 4 single-float "f")

(define-vecn 2 double-float "d")
(define-vecn 3 double-float "d")
(define-vecn 4 double-float "d")

#|
(defmacro define-vec-op (name func &rest args)
  `(defun ,name (,@args)
     (cl:map (type-of ,(car args))
             ,func
             ,@args)))
|#

;; (declaim (ftype (function (single-float single-float single-float) single-float) sfclamp))
;; (defun sfclamp (value low high)
;;   ""
;;   (declare (optimize (speed 3) (safety 0)))
;;   (min high (max low value)))

;; (declaim (ftype (function (vec2 vec2) vec2) vec2-add))
;; (defun vec2-add (v1 v2)
;;   (declare (optimize (speed 3) (safety 0)))
;;   (cl:map 'vec2 #'+ v1 v2))

;; (declaim (ftype (function (vec2 single-float) vec2) vec2-mul))
;; (defun vec2-mul (v1 f)
;;   (declare (optimize (speed 3) (safety 0)))
;;   (cl:map 'vec2 (lambda (x) (* x f)) v1))

;; (declaim (ftype (function (vec2 single-float) vec2) vec2-div))
;; (defun vec2-div (v1 f)
;;   (vec2-mul v1 (/ 1.0 f)))

;; (declaim (ftype (function (vec2 vec2) vec2) vec2-add))
;; (defun vec2-sub (v1 v2)
;;   (declare (optimize (speed 3) (safety 0)))
;;   (cl:map 'vec2 #'+ (the vec2 v1) (vec2-mul v2 -1.0)))

;; (declaim (ftype (function (vec2 vec2 vec2) vec2) vec2-clamp))
;; (defun vec2-clamp (value low high)
;;   (declare (optimize (speed 3) (safety 0)))
;;   (cl:map 'vec2 #'clamp value low high))

(defun x-val (vec)
  "The first value of VEC."
  (aref vec 0))
(defun y-val (vec)
  "The second value of VEC."
  (aref vec 1))
(defun z-val (vec)
  "The third value of VEC."
  (aref vec 2))
(defun w-val (vec)
  "The fourth value of VEC."
  (aref vec 3))

(defun (setf x-val) (value vec)
  (setf (aref vec 0) value))
(defun (setf y-val) (value vec)
  (setf (aref vec 1) value))
(defun (setf z-val) (value vec)
  (setf (aref vec 2) value))
