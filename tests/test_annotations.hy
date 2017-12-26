(require [src.utils.macros [*]])
(require [hy.extra.anaphoric [*]])
(require [tests.hytest [*]])
(import [tests.hytest [*]])

(import
  pytest

  [src.annotations [annotate]]
  [src.models [Candidate]])


;; * Tests

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

(defn test-annotate-module-and-aliases []
  (import itertools)
  (assert= "<module itertools>"
           (-> "itertools"
             (Candidate :local (locals))
             annotate))

  (import [itertools :as it])
  (assert= "<module it>"
           (-> "it"
             (Candidate :local (locals))
             annotate)))

(defn test-annotate-vars []
  (setv doesnt-exist False)
  (assert= "<instance doesnt-exist>"
           (-> "doesnt-exist"
             (Candidate :local (locals))
             annotate)))

(defn test-annotate-compiler []
  (assert= "<compiler try>"
           (-> "try" Candidate annotate)))

(defn test-annotate-shadow []
  (assert= "<shadowed is>"
           (-> "is" Candidate annotate))
  (assert= "<shadowed get>"
           (-> "get" Candidate annotate)))

(defn test-annotate-macro []
  (assert= "<macro ->>"
           (-> "->" Candidate annotate))
  (assert= "<macro as->>"
           (-> "as->" Candidate annotate)))
