;;;; err.asd

(asdf:defsystem #:err
  :description "errr"
  :author "Ed Ye <hahahadude@gmail.com>"
  :license "Licenseless Rider"
  :depends-on (#:alexandria
               #:cl-glfw3
               #:cl-opengl
               #:cl-soil
               #:fset
               #:glkit
               #:trivial-garbage
               #:defenum
               #:swank
               #:ironclad
               #:cl-freetype2)
  :serial t
  :components ((:file "package")
               (:file "global")
               (:file "timer")
               (:file "utils")
               (:file "time-travel")
               (:file "event")
               (:file "input")
               (:file "program")
               (:file "texture2d")
               (:file "resource-manager")
               (:file "program-manager")
               (:file "texture-manager")
               (:file "font-manager")
               (:file "drawer")
               (:file "sprite-drawer")
               (:file "rect-drawer")
               (:file "text-drawer")
               (:file "entity")
               (:file "err")))

