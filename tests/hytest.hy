(require [src.utils.macros [*]])
(require [hy.extra.anaphoric [*]])


(defn assert= [x y]
  (assert (= x y)))
