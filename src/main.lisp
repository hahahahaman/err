(in-package #:err)

(defmacro err-run (title
                   &key
                     init-code
                     input-code
                     render-code
                     update-code
                     cleanup-code)
  ;; In case the OS's default library search algorithm cannot find
  ;; the correct libraries, libraries will be provided
  ;; not sure how this works
  ;; (pushnew #P"./" *foreign-library-directories*
  ;;          :test #'equal)
  "Generic game loop code."

  ;; Graphics calls on OS X must occur in the main thread
  ;; (trivial-main-thread:with-body-in-main-thread ())

  ;; block used to return out of function call
  `(run-thread
     (block nil
       (glfw:with-init-window (:title ,title
                               :width *width*
                               :height *height*
                               :opengl-forward-compat t
                               :opengl-profile :opengl-core-profile
                               :context-version-major 3
                               :context-version-minor 3
                               :decorated t
                               :resizable nil
                               ;;full screen mode
                               ;; :monitor (glfw:get-primary-monitor)
                               ;; :refresh-rate 60
                               )
         ;; (glfw:swap-interval 1)
         (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address)

         (unless (gl::features-present-p (>= :glsl-version 3.3))
           (return nil))

         ;; initialize
         (initialize-globals)
         ,init-code

         ;; glfw input
         (glfw:set-key-callback 'key-callback)
         (glfw:set-mouse-button-callback 'mouse-callback)
         (glfw:set-cursor-position-callback 'cursor-callback)
         (glfw:set-scroll-callback 'scroll-callback)
         ;; (glfw:set-input-mode :cursor :disabled) ;; hides cursor

         (iter (until (glfw:window-should-close-p))

           ;; running loop in seperate thread means swank still runs
           ;; (update-swank)

           (continuable
             (glfw:poll-events)

             ,input-code

             ,render-code
             ,update-code

             (update-files)
             (update-events)
             (glfw:swap-buffers)
             (update-globals)))

         ,cleanup-code))))
