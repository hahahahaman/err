(in-package :err)

;; original code structure from mathkit
(defmacro define-vecn (n type &optional (suffix ""))
  "Creates a new type of vector, with corresponding utility functions."
  (let* ((vecn (alexandria:symbolicate 'vec (format nil "~A" n)
                                       (string-upcase suffix)))
         (v+ (alexandria:symbolicate vecn "+"))
         (v- (alexandria:symbolicate vecn "-"))
         (v* (alexandria:symbolicate vecn "*"))
         (v/ (alexandria:symbolicate vecn "/"))
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

         (declaim (ftype (function (,vecn ,vecn) ,vecn) ,vmul))
         (defun ,vmul (v1 v2)
           (declare (optimize (speed 3) (safety 0)))
           (,vecn ,@(iter (for i from 0 below n)
                      (collect `(* (aref v1 ,i) (aref v2 ,i))))))

         (declaim (ftype (function (,vecn ,vecn) ,vecn) ,vdiv))
         (defun ,vdiv (v n)
           (declare (optimize (speed 3) (safety 0)))
           (,vecn ,@(iter (for i from 0 below n)
                      (collect `(/ (aref v1 ,i) (aref v2 ,i))))))

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

(defmacro define-vec-n-val (func-name n)
  `(progn
     (declaim (ftype (function (vector) number) ,func-name))
     (defun ,func-name (vec)
       (declare (optimize (speed 3) (safety 0)))
       (aref vec ,n))

     (defun (setf ,func-name) (value vec)
       (setf (aref vec ,n) value))))

(define-vec-n-val x-val 0)
(define-vec-n-val y-val 1)
(define-vec-n-val z-val 2)
(define-vec-n-val w-val 3)
