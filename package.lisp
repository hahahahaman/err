;;;; package.lisp

(defpackage #:err
  (:use #:cl #:iter #:fset)
  (:import-from :kit.glm #:vec2 #:vec3 #:vec4)
  (:shadowing-import-from :iter #:iter #:while)
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
  (:export
   ;; globals
   *global-setfs*
   defglobal

   ;; window dimensions
   *width*
   *height*

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

   ;; drawers
   *sprite-drawer*
   *rect-drawer*

   ;;entities
   *entities*

   ;;; utils
   :square
   :cube
   :dist-mod

   :average-fps
   :cap-fps

   :update-dt
   :clear-actions
   :update-globals
   :initialize-globas

   :update-window-title

   :concat-vecs
   :sequence-to-gl-array
   :with-sequence-to-gl-array
   :random-in-range
   :cfloat
   :sizeof
   :sizeof*

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

   :continuable
   :update-swank

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

   :add-event
   :update-events

   :key-action-p
   :key-pressed-p
   :mouse-button-action-p
   :mouse-button-pressed-p

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

   :resource-manager
   :program-manager
   :texture-manager
   :load-resource
   :get-resource
   :clear-resources
   :load-resource
   :load-texture
   :load-program
   :get-resource
   :get-texture
   :get-program

   :drawer
   :sprite-drawer
   :sprite-draw
   :rect-drawer
   :rect-draw

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
   :find-entity-by-component)
  )

