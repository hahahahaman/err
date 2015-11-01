### err 

#### Intro

Some code I reuse for games. There is possibly some emphasis on iterative
development, but it's still young and convoluted.

#### Example

Currently an Opengl window can be opened like this

```lisp
(defun initialize ())
(defun handle-input ())
(defun render ())
(defun update ())
(defun cleanup ())

(defun example ()
  (err:run "insert-title"
           :init-code (initialize)
           :input-code (handle-input)
           :render-code (render)
           :update-code (update)
           :cleanup-code (cleanup)))
```
