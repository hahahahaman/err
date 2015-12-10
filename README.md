## err - engine really rong

### Intro

Some code I reuse for games. There is possibly some emphasis on iterative
development, but it's still young and convoluted.

### Example

Currently an Opengl window can be opened like this:

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

See [added-text](https://github.com/hahahahaman/added-text) for a working project.

### Installation

err is known to work with [SBCL](http://www.sbcl.org/) and
[Clozure](http://ccl.clozure.com/) on Linux and Windows (get libraries with
[win-builds](http://win-builds.org/doku.php)).

1. [Get Common Lisp](http://cliki.net/Getting+Started), remember to install [quicklisp](https://www.quicklisp.org/beta/).

2. Get dependencies, either through quicklisp or from their repositories.
  * [alexandria](https://common-lisp.net/project/alexandria/)
  * [iterate](https://common-lisp.net/project/iterate/)
  * [cl-glfw3](https://github.com/AlexCharlton/cl-glfw3)
  * [cl-opengl](https://github.com/3b/cl-opengl)
  * [glkit](https://github.com/lispgames/glkit)
  * [cl-freetype2](https://github.com/rpav/cl-freetype2)
  * [fest](https://github.com/slburson/fset)
  * [defenum](http://defenum.sourceforge.net/)
  * [ironclad](http://method-combination.net/lisp/ironclad/)
  * swank (part of [slime](https://common-lisp.net/project/slime/))

   Not in quicklisp (get from their repositories):
      * [cl-soil](https://github.com/cbaggers/cl-soil)

3. clone this project into the quicklisp/local-projects/ (by default in your
   user home folder):

  ```
  $ cd ~/quickload/local-projects/

  $ git clone https://github.com/hahahahaman/err
  ```

  Something I like to do instead of step 4 is clone the project into
  ~/quicklisp/local-projects/ then
4. add ASD project file

  Add the relative path of the asd file into system-index.txt (also in the
  local-projects folder):

  ```
  err/err.asd

  err/err-examples.asd
  ```

  OR

  launch Common Lisp, add project to asdf, and load err:

  ```lisp
  $ sbcl

  * (push #p"/path/to/err" asdf:*central-registry*)

  * (ql:quickload :err)
  ```

  OR

  more ways found
  [here](http://stackoverflow.com/questions/11261045/how-to-add-a-local-project-to-asdf-configured-by-quicklisp).
