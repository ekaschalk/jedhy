(require [src.utils.macros [*]])
(require [hy.extra.anaphoric [*]])
(require [tests.hytest [*]])
(import [tests.hytest [*]])

(import
  pytest

  [src.models [Candidate Prefix]])


;; * Prefixes

(deffixture prefixes
  "Prefixes, their candidate symbol, and attr-prefix."
  [["obj" "" "obj"]
   ["obj.attr" "obj" "attr"]
   ["obj." "obj" ""]
   ["obj.attr." "obj.attr" ""]
   ["" "" ""]]
  (#%[(Prefix %1) %2 %3]
     #* it))

(with-fixture prefixes
  test-split-prefix [prefix candidate attr-prefix]

  (assert= prefix.candidate.symbol candidate)
  (assert= prefix.attr-prefix attr-prefix))

;; * Candidates
;; ** Compiler

(defn test-candidate-compiler []
  (setv compiler?
        (fn-> Candidate (.compiler?)))

  (assert (->> ["try" "for*" "+=" "require"]
            (map compiler?)
            all))
  (assert (->> "doesn't exist"
            compiler?
            none?)))

;; ** Macros

(defn test-candidate-macro []
  (setv macro?
        (fn-> Candidate (.macro?)))

  (assert (->> ["->" "with" "when"]
            (map macro?)
            all))
  (assert (->> "doesn't exist"
            macro?
            none?)))

;; ** Shadows

(defn test-candidate-shadow []
  (setv shadow?
        (fn-> Candidate (.shadow?)))

  (assert (->> ["get" "is" "is_not"]
            (map shadow?)
            all))
  (assert (->> "doesn't exist"
            shadow?
            none?)))

;; ** Python

(defn test-candidate-evaled-fails []
  (assert (none? (-> "doesn't exist" Candidate (.evaled?)))))

(defn test-candidate-evaled-builtins []
  (assert (= print (-> "print" Candidate (.evaled?)))))

(defn test-candidate-evaled-methods []
  (assert (= print.--call-- (-> "print.--call--" Candidate (.evaled?)))))

(defn test-candidate-evaled-modules []
  (import builtins)
  (assert (= builtins (-> "builtins" Candidate (.evaled?)))))

;; ** Attributes

(defn test-candidate-attributes-fails []
  (assert (none? (-> "doesn't exist" Candidate (.attributes)))))

(defn test-candidate-attributes-builtin []
  (assert-all-in ["--str--" "--call--"]
                 (-> "print" Candidate (.attributes))))

(defn test-candidate-attributes-module []
  (import builtins)
  (assert-all-in ["eval" "AssertionError"]
                 (-> "builtins" Candidate (.attributes))))

(defn test-candidate-attributes-nested []
  (assert-all-in ["--str--" "--call--"]
                 (-> "print.--call--" Candidate (.attributes))))

;; ** Namespacing

(defn test-candidate-namespace-globals []
  (import itertools)
  (assert (none?
            (-> "itertools.chain"
              Candidate
              (.attributes))))
  (assert-in "from-iterable"
             (-> "itertools.chain"
               (Candidate :namespace (globals))
               (.attributes))))

(defn test-candidate-namespace-locals []
  (defclass AClass [])
  (assert (none?
            (-> "AClass"
              Candidate
              (.attributes))))
  (assert (none?
            (-> "AClass"
              (Candidate :namespace (globals))
              (.attributes))))
  (assert-in "--doc--"
             (-> "AClass"
               (Candidate :local (locals))
               (.attributes)))

  (setv doesnt-exist 1)
  (assert (-> "doesnt-exist"
            (Candidate :local (locals))
            (.evaled?))))
