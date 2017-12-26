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

;; FAIL evaled? isn't using current namespace
;; (defn test-annotate-class []
;;   (import itertools)
;;   (assert= "<class itertools.tee>"
;;            (-> "itertools.tee" Candidate annotate)))
