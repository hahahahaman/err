(in-package #:err)

;;; base class for things that draw

(defclass drawer ()
  ((program
    :type program
    :accessor program
    :initarg :program)
   (vao
    :type unsigned-byte
    :accessor vao
    :initarg :vao))
  (:default-initargs
   :program nil
   :vao (gl:gen-vertex-array)))
