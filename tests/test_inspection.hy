(require [src.utils.macros [*]])
(require [hy.extra.anaphoric [*]])
(require [tests.hytest [*]])
(import [tests.hytest [*]])

(import
  [src.inspection [Parameter
                   Signature
                   Inspect
                   builtin-docs-to-lispy-docs]])

;; NOTE kwwonly params order isn't preserved atm (its f h [g 1])
;; This might be ok though

(defn test-signature []
  (defn func [a b &optional c [d 1] e &kwonly f [g 1] h])

  (setv sig
        (Signature func))

  (print sig))
