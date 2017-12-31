(require [src.utils.macros [*]])
(require [hy.extra.anaphoric [*]])
(require [tests.hytest [*]])
(import [tests.hytest [*]])

(import
  [src.namespace [Namespace]])


(defn test-namespace-macros []
  (setv namespace
        (Namespace))

  ;; (print (namespace.macros.keys))
  )
