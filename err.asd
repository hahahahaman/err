;;;; err.asd

(asdf:defsystem #:err
  :description "errr"
  :author "Ed Ye <hahahadude@gmail.com>"
  :license "Licenseless Rider"
  :depends-on (#:alexandria
               #:bordeaux-threads
               #:iterate
               #:cl-fad
               #:cl-glfw3
               #:cl-opengl
               #:cl-soil
               #:cl-freetype2
               #:fset
               #:glkit
               #:trivial-garbage
               #:defenum
               #:swank
               #:ironclad)
  :serial t
  :components ((:module src
                :components ((:file "package")
                             (:file "globals")
                             (:file "timer")
                             (:file "vec")
                             (:file "utils")
                             (:file "time-travel")
                             (:file "file-tracker")
                             (:file "event")
                             (:file "input")
                             (:file "program")
                             (:file "texture2d")
                             (:file "resource-manager")
                             (:file "program-manager")
                             (:file "texture-manager")
                             (:file "font-manager")
                             (:file "vertices")
                             (:file "camera")
                             (:file "drawer")
                             (:file "sprite-drawer")
                             (:file "rect-drawer")
                             (:file "text-drawer")
                             (:file "cube-drawer")
                             (:file "entity")
                             (:file "main")
                             (:file "err")))))

