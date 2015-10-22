;;;; err.asd

(asdf:defsystem #:err
  :description "errr"
  :author "Ed <hahahadude@gmail.com>"
  :license "Licenseless Rider"
  :depends-on (#:alexandria
               #:cl-glfw3
               #:cl-opengl
               #:cl-soil
               #:fset
               #:glkit
               #:trivial-garbage
               #:defenum
               #:swank)
  :serial t
  :components ((:file "package")
               (:file "global")
               (:file "utils")
               (:file "time-travel")
               (:file "event")
               (:file "input")
               (:file "program")
               (:file "texture2d")
               (:file "resource-manager")
               (:file "drawer")
               (:file "entity")
               (:file "err")))

