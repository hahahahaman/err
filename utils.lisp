(in-package #:err)

;;; utils

(defun square (x)
  (* x x))
(defun cube (x)
  (* x x x))

(defun dist-mod (a b modulo)
  "Distance between A and B, when mod."
  (min (mod (- a b) modulo) (mod (- b a) modulo)))

;; fps
(let* ((max-samples 500)
       (samples (make-array max-samples
                            :element-type 'double-float
                            :initial-element (/ 1.0d0 +max-fps+)))
       (sample-index 0))
  (defun average-fps ()
    (setf (aref samples sample-index) *dt*
          sample-index (mod (1+ sample-index) max-samples))
    ;; (print *dt*)
    (/ max-samples (reduce #'+ samples))))

(defun cap-fps ()
  "Cap frame rate, preventing resource hogging"
  (let ((frame-diff (- (+ (/ 1.0d0 +max-fps+) *previous-time*) (glfw:get-time))))
    (when (> frame-diff 0)
      (sleep frame-diff))))

;;;;;;;;;;;;;;;;;
;; handle globals
;;;;;;;;;;;;;;;;;

(defun update-dt ()
  "Called in the main loop, updates time globals."
  (setf *dt* (- (glfw:get-time) *previous-time*)
        *previous-time* (glfw:get-time))

  ;; (incf *total-frames*)

  ;; prevent unruly time steps from breaking game
  (setf *dt* (max 0.0d0 (min 0.25d0 *dt*))))

(defun clear-actions ()
  "Clears the input actions from last frame."
  (setf *key-actions* nil
        *mouse-button-actions* nil
        *scroll-callback-p* nil
        *cursor-callback-p* nil))

(defun update-globals ()
  "A single function that encompasses global updates"
  (clear-actions)
  (update-dt)
  (cap-fps))

(defun initialize-globals ()
  ;; random seed
  (setf *random-state* (make-random-state t))

  ;; go through all globals setting them to original value
  (iter (for (var-symbol func) on *global-setfs* by #'cddr)
    (funcall func)))

;;; type utils

(defun concat-vecs (&rest vecs)
  "Creates of single-float simple-array from lone values, lists, and/or vectors."
  (let* ((len 0) ;; keeps track of simple-array length
         (vec (apply #'concatenate ;; combine all sequences, lists and vectors
                     'vector ;; output a vector

                     ;; ensure all of the elements of VECS are of type sequence
                     (mapcar
                      (lambda (x)
                        (cond ((typep x 'sequence)
                               (incf len (length x))
                               x) ;; keep track of length
                              (t
                               (incf len 1)
                               (list (coerce x 'single-float)))))
                      vecs))))
    ;; finally output simple-array
    (coerce vec `(simple-array single-float (,len)))))

(defun sequence-to-gl-array (sequence type)
  "Creates a gl-array from a lisp sequence, with elements of type TYPE.
Remember to free gl-array afterwards."
  (let ((gl-arr (gl:alloc-gl-array type (length sequence))))
    (dotimes (i (length sequence)
                gl-arr)
      (setf (gl:glaref gl-arr i) (elt sequence i)))))

(defmacro with-sequence-to-gl-array ((var sequence type) &body body)
  `(let ((,var (sequence-to-gl-array ,sequence ,type)))
     ,@body
     (gl:free-gl-array ,var)))

(defun random-in-range (start end)
  "Random number between start and end inclusive."
  (+ start (random (- (1+ end) start))))

(declaim (ftype (function (real) single-float) cfloat))
(defun cfloat (n)
  (declare (optimize (speed 3) (safety 0)))
  "Coerce N to single-float. Just makes the function shorter."
  (coerce n 'single-float))

(defun sizeof (type)
  "Gives to foreign-type-size of TYPE. Used with cffi stuff, like opengl."
  (cffi-sys:%foreign-type-size type))

(defun sizeof* (type multiple)
  "Multiply sizeof TYPE, by MULTIPLE"
  (* (sizeof type) multiple))

;;;;;;;;;;;
;; vectors
;;;;;;;;;;;

;; code from mathkit
(defmacro define-vecn (n type &optional (prefix ""))
  (let ((vecn (alexandria:symbolicate (string-upcase prefix)
                                      'vec (format nil "~A" n))))
    `(progn
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
              a+)))))))

(define-vecn 2 integer "i")
(define-vecn 3 integer "i")
(define-vecn 4 integer "i")

#|
(defmacro define-vec-op (name func &rest args)
  `(defun ,name (,@args)
     (cl:map (type-of ,(car args))
             ,func
             ,@args)))
|#

(defun vec-add (v1 v2)
  ;; (declare (optimize (speed 3) (safety 0)))
  "Returns a vector of the same type as V1, which is a component-wise sum of
V1 and V2."
  (cl:map (type-of v1) #'+ v1 v2))

(defun vec-mul (v1 f)
  "Returns a vector with the same type as V1, which has components multiplied by F."
  (cl:map (type-of v1)
          (lambda (x) (* (the single-float x) (the single-float f)))
          v1))

(defun vec-div (v1 f)
  (vec-mul v1 (/ 1.0 f)))

(defun vec-length (v)
  (sqrt (reduce #'+ (cl:map (type-of v) #'square v))))

(defun clamp (value low high)
  (declare (optimize (speed 3) (safety 0)))
  (min high (max low value)))

(declaim (ftype (function (single-float single-float single-float) single-float) sfclamp))
(defun sfclamp (value low high)
  (declare (optimize (speed 3) (safety 0)))
  (min high (max low value)))

(defun vec-clamp (value low high)
  (cl:map (type-of value) #'sfclamp value low high))

(declaim (ftype (function (vec2 vec2) vec2) vec2-add))
(defun vec2-add (v1 v2)
  (declare (optimize (speed 3) (safety 0)))
  (cl:map 'vec2 #'+ v1 v2))

(declaim (ftype (function (vec2 single-float) vec2) vec2-mul))
(defun vec2-mul (v1 f)
  (declare (optimize (speed 3) (safety 0)))
  (cl:map 'vec2 (lambda (x) (* x f)) v1))

(declaim (ftype (function (vec2 single-float) vec2) vec2-div))
(defun vec2-div (v1 f)
  (vec2-mul v1 (/ 1.0 f)))

(declaim (ftype (function (vec2 vec2) vec2) vec2-add))
(defun vec2-sub (v1 v2)
  (declare (optimize (speed 3) (safety 0)))
  (cl:map 'vec2 #'+ (the vec2 v1) (vec2-mul v2 -1.0)))

(declaim (ftype (function (vec2 vec2 vec2) vec2) vec2-clamp))
(defun vec2-clamp (value low high)
  (declare (optimize (speed 3) (safety 0)))
  (cl:map 'vec2 #'sfclamp value low high))

(defun x-val (vec)
  (aref vec 0))
(defun y-val (vec)
  (aref vec 1))
(defun z-val (vec)
  (aref vec 2))
(defun w-val (vec)
  (aref vec 3))

;;; slots

(defmacro get-slot (object &rest nested-slot-names)
  (iter (iter:with current = object)
    (for s in nested-slot-names)
    (setf current `(slot-value ,current ,s))
    (finally (return current))))

;;; file io

(defun read-sexp-from-file (filename)
  "Reads all sexp from file FILENAME, returning a list of all collected."
  (with-open-file (file filename :direction :input)
    (with-standard-io-syntax
      (let ((*read-eval* nil))
        (iter (for sexp = (read file nil))
          (while sexp)
          (collect sexp))))))

;;; swank stuff

(defmacro continuable (&body body)
  "Helper macro that we can use to allow us to continue from an
  error. Remember to hit C in slime or pick the restart so errors don't kill the app."
  `(restart-case
       (progn ,@body) (continue () :report "Continue")))

(defun update-swank ()
  (ignore-errors
   "Called from within the main loop, this keep the lisp repl running"
   (continuable
    (let ((connection (or swank::*emacs-connection* (swank::default-connection))))
      (when connection
        (swank::handle-requests connection t))))))

;; copy instances

(defgeneric copy-instance (object &rest initargs &key &allow-other-keys)
  (:documentation "Makes and returns a shallow copy of OBJECT.

  An uninitialized object of the same class as OBJECT is allocated by
  calling ALLOCATE-INSTANCE.  For all slots returned by
  CLASS-SLOTS, the returned object has the
  same slot values and slot-unbound status as OBJECT.

  REINITIALIZE-INSTANCE is called to update the copy with INITARGS.")
  (:method ((object standard-object) &rest initargs &key &allow-other-keys)
    (let* ((class (class-of object))
           (copy (allocate-instance class)))
      (dolist (slot-name (mapcar #'sb-mop:slot-definition-name
                                 (sb-mop:class-slots class)))
        (when (slot-boundp object slot-name)
          (setf (slot-value copy slot-name)
                (slot-value object slot-name))))
      (apply #'reinitialize-instance copy initargs))))

;; Threading macro from clojure
(defmacro -> (x &optional (form nil form-supplied-p) &rest more)
  (if form-supplied-p
      (if more
          `(-> (-> ,x ,form) ,@more)
          (if (listp form)
              `(,(car form) ,x ,@(cdr form))
              (list form x)))
      x))

;; list utils

(defun drop-nth (n list)
  (append (subseq list 0 n) (nthcdr (1+ n) list)))

(defun add-nth (n elem list)
  (append (subseq list 0 n) (cons elem (nthcdr n list))))

(defun set-nth (n elem list)
  (append (subseq list 0 n) (cons elem (nthcdr (1+ n) list))))

(defun plist-set (place indicator value)
  (do ((plist place (cddr plist))
       (n 0 (+ 2 n)))
      ((null plist) (append place (list indicator value)))
    (cond ((atom (cdr plist))
           (error 'simple-type-error
                  :format-control "malformed property list: ~S."
                  :format-arguments (list place)
                  :datum (cdr plist)
                  :expected-type 'cons))
          ((eq (car plist) indicator)
           (return (append (subseq place 0 (1+ n))
                           (cons value (nthcdr (+ n 2) place))))))))

;;; fset functions

(defun get-map-keys (to-type map)
  (image (lambda (x) (car x)) (convert to-type map)))
(defun get-map-values (to-type map)
  (image (lambda (x) (cdr x)) (convert to-type map)))

;; destructive fset stuff

(defmacro with! (collection value1 &optional value2)
  `(setf ,collection (with ,collection ,value1 ,value2)))
(defmacro less! (collection value1 &optional value2)
  `(setf ,collection (less ,collection ,value1 ,value2)))

;;;
;;; Garbage Collection
;;;

(defun gc (&key full verbose)
  (trivial-garbage:gc :full full :verbose verbose))

;;;
;;; build file
;;;

;; (defun make-build-file (path entry &key system output))

;;;
;;; md5
;;;

(defun md5 (str)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :md5 
                             (ironclad:ascii-string-to-byte-array str))))
(defun valid-md5? (checksum str)
  (string= checksum (md5 str)))
