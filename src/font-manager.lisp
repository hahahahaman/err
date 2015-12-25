(in-package :err)

(defclass font-manager (resource-manager)
  ())

(defun load-font (name pathname &optional (pixel-size 14) (manager *font-manager*))
  (ft2:with-open-face (face pathname 0 (ft2:make-freetype))
    (ft2:set-pixel-sizes face 0 pixel-size)

    ;; disable alignment restriction
    (gl:pixel-store :unpack-alignment 1)

    ;; first 128 ASCII chars
    (let ((text-chars (empty-map)))
      (iter (for c from 0 below 256)
        (multiple-value-bind (bitmap advance left top)
            (freetype2:default-load-render face (code-char c) nil)
          (let ((texture (first (gl:gen-textures 1)))
                (width (freetype2-types:ft-bitmap-width bitmap))
                (rows (freetype2-types:ft-bitmap-rows bitmap)))
            (gl:bind-texture :texture-2d texture)
            (gl:tex-image-2d
             :texture-2d
             0
             :red
             width
             rows
             0
             :red
             :unsigned-byte
             (freetype2-types:ft-bitmap-buffer bitmap))

            (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
            (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
            (gl:tex-parameter :texture-2d :texture-min-filter :linear)
            (gl:tex-parameter :texture-2d :texture-mag-filter :linear)

            (with! text-chars (code-char c)
                   (make-text-char :texture-id texture
                                   :size (vec2i width rows)
                                   :bearing (vec2i left top)
                                   :advance advance)))))
      (gl:bind-texture :texture-2d 0)

      (load-resource name text-chars manager))))

(defun get-font (name &optional (manager *font-manager*))
  (get-resource name manager))
