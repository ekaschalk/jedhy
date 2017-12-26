(require [src.utils.macros [*]])
(require [hy.extra.anaphoric [*]])
(require [tests.hytest [*]])
(import [tests.hytest [*]])

(import
  pytest

  [src.annotations [annotate]]
  [src.models [Candidate]])


(defn test-annotate-builtin-or-function []
  (assert= "<def print>"
           (-> "print" Candidate annotate))
  (assert= "<def first>"
           (-> "first" Candidate annotate)))

(defn test-annotate-class []
  (defclass AClass [])
  (assert= "<class AClass>"
           (-> "AClass"
              (Candidate :local (locals))
              annotate)))
