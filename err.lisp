(in-package :err)

;; exports all symbols in package
;; seems reckless, but convenient
(let ((pack (find-package :err)))
  (do-all-symbols (sym pack)
    (when (eql (symbol-package sym) pack)
      (export sym))))
