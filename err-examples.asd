;;;; err-examples.asd

(asdf:defsystem #:err-examples
  :description "Some examples of err."
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
