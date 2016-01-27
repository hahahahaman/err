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
    "=> FLOAT
Average time change over a few hundred frames."
    (setf (aref samples sample-index) *dt*
          sample-index (mod (1+ sample-index) max-samples))
    (/ max-samples (reduce #'+ samples))))

(defun limit-fps ()
  "Limits frame rate, preventing resource hogging."

  ;; when the frame occurred faster than the max frame time length
  ;; sleep for that difference
  (let* ((max-frame-time-length (+ (/ 1.0d0 +max-fps+) *previous-time*))
         (current-time (glfw:get-time))
         (sleep-time (- max-frame-time-length current-time)))
    (when (> sleep-time 0.0)
      (sleep sleep-time))))

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
  "A single function that encompasses global updates."
  (clear-actions)
  (update-dt)
  (limit-fps))

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
  "Random number between start and end, inclusive."
  (if (> start end)
      (random-in-range end start)
      (+ start
         (let* ((diff (- end start))
                (randy (random (+ diff
                                  ;; increase limit if numbers are integers
                                  ;; to include end
                                  (if (integerp diff) 1 0)))))
           (if (floatp randy)
               ;; round randy to diff in case it is super close to diff
               (if (<= (- diff randy) single-float-epsilon)
                   diff
                   randy)
               randy)))))

(declaim (ftype (function (real) single-float) cfloat))
(defun cfloat (n)
  "Coerce N to single-float. Just makes the function shorter."
  (declare (optimize (speed 3) (safety 0)))
  (coerce n 'single-float))

(defun sizeof (type)
  "Gives to foreign-type-size of TYPE. Used with cffi stuff, like cl-opengl."
  (cffi-sys:%foreign-type-size type))

(defun sizeof* (type multiple)
  "Multiply sizeof TYPE by MULTIPLE"
  (* (sizeof type) multiple))

;;; slots

(defmacro get-slot (object &rest nested-slot-names)
  "Returns a nested slot-value form."
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
  "Helper macro that we can use to allow us to continue from an error. Remember
  to hit C (for continue) in slime or pick the restart so errors don't kill the
  app."
  `(restart-case
       (progn ,@body) (continue () :report "Continue")))

(defun update-swank ()
  (ignore-errors
   "Called from within the main loop, this keep the lisp repl running."
   (continuable
     (let ((connection (or swank::*emacs-connection* (swank::default-connection))))
       (when connection
         (swank::handle-requests connection t))))))

;; copy instances

;; (defgeneric copy-instance (object &rest initargs &key &allow-other-keys)
;;   (:documentation "Makes and returns a shallow copy of OBJECT.

;;   An uninitialized object of the same class as OBJECT is allocated by
;;   calling ALLOCATE-INSTANCE.  For all slots returned by
;;   CLASS-SLOTS, the returned object has the
;;   same slot values and slot-unbound status as OBJECT.

;;   REINITIALIZE-INSTANCE is called to update the copy with INITARGS.")
;;   (:method ((object standard-object) &rest initargs &key &allow-other-keys)
;;     (let* ((class (class-of object))
;;            (copy (allocate-instance class)))
;;       (dolist (slot-name (mapcar #'sb-mop:slot-definition-name
;;                                  (sb-mop:class-slots class)))
;;         (when (slot-boundp object slot-name)
;;           (setf (slot-value copy slot-name)
;;                 (slot-value object slot-name))))
;;       (apply #'reinitialize-instance copy initargs))))

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

;; TODO
;; (defun make-build-file (path entry &key system output))

;;;
;;; md5
;;;

(defun md5 (str)
  "=> CHECKSUM
Returns the md5 checksum of STR."
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :md5 
                             (ironclad:ascii-string-to-byte-array str))))
(defun valid-checksum-p (checksum other-checksum)
  "=> BOOLEAN
Checks if two checksums are equal."
  (string= checksum other-checksum))

;;;
;;; some math
;;;
;; (defun radians->degrees (radians)
;;   (* (/ 180 pi) radians))

;; (defun degrees->radians (degrees)
;;   (* (/ pi 180) degrees))

(defun rotate-point-ccw (x y radians)
  (values
   (+ (* x (cos radians)) (- (* y (sin radians))))
   (+ (* x (sin radians)) (* y (cos radians)))))

(defun rect-in-rectangle-p (x y width height o-top o-left o-width o-height)
  "Nice to have for on the spot rectangle coliision."
  (declare (single-float x y width height o-top o-left o-width o-height)
           (optimize (speed 3)))
  (not (or
        (<= (+ o-top o-height) y)
        (<= (+ y height) o-top)
        (<= (+ x width) o-left)
        (<= (+ o-left o-width) x))))
