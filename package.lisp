;;;; package.lisp

(defpackage #:err
  (:use #:cl #:iterate #:fset)
  (:import-from :kit.glm #:vec2 #:vec3 #:vec4)
  (:shadowing-import-from :iterate #:iter #:while)
  (:shadowing-import-from :fset
                          ;; Shadowed type/constructor names
                          #:set #:map
                          ;; Shadowed set operations
                          #:union #:intersection #:set-difference #:complement
                          ;; Shadowed sequence operations
                          #:first #:last #:subseq #:reverse #:sort #:stable-sort
                          #:reduce
                          #:find #:find-if #:find-if-not
                          #:count #:count-if #:count-if-not
                          #:position #:position-if #:position-if-not
                          #:remove #:remove-if #:remove-if-not
                          #:substitute #:substitute-if #:substitute-if-not
                          #:some #:every #:notany #:notevery
                          #:with)
  #|(:export
  ;; globals
  *global-setfs*
  :defglobal

  ;; window dimensions
  *width*
  *height*

  *debug*

  ;; fps
  +max-fps+
  *dt*
  *previous-time*
  ;; input
  *key-actions*
  *mouse-button-actions*
  *key-pressed*
  *mouse-button-pressed*
  *cursor-callback-p*
  *first-mouse*
  *cursor-x*
  *cursor-y*
  *last-x*
  *last-y*
  *scroll-callback-p*
  *scroll-x*
  *scroll-y*

  ;; time debug
  *enum-time-travel-state*
  +time-play+
  +time-paused+
  +time-rewind+
  +time-forward+
  *time-travel-state*
  *current-frame*
  *max-frame-index*
  *timeline*
  *tracked-vars*

  ;; events
  *destructive-changes*

  ;; resource managers
  *program-manager*
  *texture-manager*
  *font-manager*

  ;; drawers
  *sprite-drawer*
  *rect-drawer*
  *text-drawer*

  ;;entities
  *entities*

  ;;file tracking
  *tracked-files*

   ;;; timer
  :timer
  :timer-end
  :timer-time
  :make-timer

  :timer-ended?
  :timer-update
  :timer-reset

   ;;; utils
  :square
  :cube
  :dist-mod

  :average-fps
  ;; :cap-fps

  ;; :update-dt
  ;; :clear-actions
  ;; :update-globals
  ;; :initialize-globals

  :concat-vecs
  :sequence-to-gl-array
  :with-sequence-to-gl-array
  :random-in-range
  :cfloat
  :sizeof
  :sizeof*

   ;;; vectors
  :define-vecn
  :ivec2
  :ivec3
  :ivec4

  :vec-add
  :vec-mul
  :vec-div
  :vec-length
  :clamp
  :sfclamp
  :vec-clamp
  :vec2-add
  :vec2-mul
  :vec3-div
  :vec2-sub
  :vec2-clamp
  :x-val
  :y-val
  :z-val
  :w-val

  :get-slot

  :read-sexp-from-file

  ;; :continuable
  ;; :update-swank

  :copy-instance

  :->

  :drop-nth
  :add-nth
  :set-nth
  :plist-set

  :get-map-keys
  :get-map-values
  :with!
  :less!

  :gc

  :md5

   ;;; timeline stuff
  :var-keyword
  :var-keyword-macro
  :track-var
  :track-vars
  :untrack-vars
  :update-timeline
  :goto-frame
  :pause-pressed
  :play-pressed
  :forward-pressed
  :forward-time
  :rewind-pressed
  :rewind-time

   ;;; file tracker
  :track-file
  :untrack-file

   ;;; events
  :add-event
  ;; :update-events

   ;;; input
  ;; glfw callback. Needed?
  ;; :key-callback
  ;; :mouse-callback
  ;; :cursor-callback
  ;; :scroll-callback

  :key-action-p
  :key-pressed-p
  :mouse-button-action-p
  :mouse-button-pressed-p

  :id
  :program
  :use
  :load-shader-file
  :program-compile
  :get-attrib
  :get-uniform
  :make-program

  :texture2d
  :texture2d-generate
  :bind
  :make-texture2d
  ;; :width
  ;; :height
  ;; :internal-format
  ;; :image-format
  ;; :wrap-s
  ;; :wrap-t

  :resource-manager
  :load-resource
  :get-resource
  :clear-resources

  :program-manager
  :load-program
  :get-program

  :texture-manager
  :load-texture
  :get-texture

  :font-manager
  :load-font
  :get-font

   ;;; drawers
  :drawer
  :vao
  :sprite-drawer
  :sprite-draw

  :rect-drawer
  :rect-draw

  :text-char
  :texture-id
  :text-char-size
  :text-char-bearing
  :text-char-advance
  :make-text-char

  :text-drawer
  :text-chars
  :vbo
  :text-draw

  :make-entity
  :add-entity
  :remove-entities
  :get-component
  :set-component
  :get-entity
  :set-entity
  :get-entity-component
  :set-entity-component
  :find-entities
  :find-entity-by-component

   ;;; main
  :run)|#
  )
