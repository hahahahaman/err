;;;; err-examples.asd

(asdf:defsystem #:err-examples
  :description "some examples"
  :author "Ed Ye"
  :license "Licenseless Rider"
  :depends-on (#:err)
  :serial t
  :components ((:module examples
                :components ((:file "package")
                             (:file "utils")
                             (:file "flock")
                             (:file "platformer")
                             (:file "pong")))))
