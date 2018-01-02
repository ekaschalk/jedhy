(require [jedhy.utils.macros [*]])
(require [hy.extra.anaphoric [*]])
(require [tests.hytest [*]])
(import [tests.hytest [*]])

(import
  [jedhy.models [Candidate Namespace Prefix]])


;; * Namespace
;; ** Components

(defn test-namespace-core-macros []
  (assert-all-in ["->" "with" "when"]
                 (. (Namespace) macros)))


(defn test-namespace-user-macros []
  ;; Due to how Hy compiles macros, we can't assert-not-in before the defmacro
  (defmacro foo-macro [x])
  (assert-in "foo-macro"
             (. (Namespace) macros)))


(defn test-namespace-imported-macros []
  (assert-in "ap-map"
             (. (Namespace) macros)))


(defn test-namespace-compiler []
  (assert-all-in ["try" "for*" "+=" "require"]
                 (. (Namespace) compile-table)))


(defn test-namespace-shadows []
  (assert-all-in ["get" "is" "not?"]
                 (. (Namespace) shadows)))

;; ** Names

(defn test-namespace-all-names []
  (assert-all-in ["HySet" "not?" "for" "->" "ap-map" "first" "print"]
                 (. (Namespace) names)))


(defn test-namespace-names-with-locals []
  (setv x False)
  (defn foo [])
  (assert-all-in ["foo" "x"]
                 (. (Namespace :locals- (locals)) names)))

;; * Prefixes
;; ** Building

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

;; ** Completion

(defn test-completion-builtins []
  (assert-in "print"
             (.complete (Prefix "prin")))
  (assert-in "ArithmeticError"
             (.complete (Prefix "Ar"))))


(defn test-completion-method-attributes []
  (assert-in "print.--call--"
             (.complete (Prefix "print.")))
  (assert-in "print.--call--"
             (.complete (Prefix "print.__c"))))


(defn test-completion-macros []
  (assert-in "->>"
             (.complete (Prefix "-"))))


(defn test-completion-compiler []
  (assert-in "try"
             (.complete (Prefix "tr"))))


(defn test-completion-core-language []
  (assert-in "iterable?"
             (.complete (Prefix "iter"))))


(defn test-completion-modules []
  (import itertools)
  (assert-in "itertools.tee"
             (.complete (Prefix "itertools.t" (Namespace :locals- (locals))))))

;; * Candidates
;; ** Compiler

(defn test-candidate-compiler []
  (setv compiler?
        (fn-> Candidate (.compiler?)))

  (assert (->> "doesn't exist"
            compiler?
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
  (assert (= builtins
             (-> "builtins"
               (Candidate (Namespace :locals- (locals)))
               (.evaled?)))))

;; ** Attributes

(defn test-candidate-attributes-fails []
  (assert (none? (-> "doesn't exist" Candidate (.attributes)))))

(defn test-candidate-attributes-builtin []
  (assert-all-in ["--str--" "--call--"]
                 (-> "print" Candidate (.attributes))))

(defn test-candidate-attributes-module []
  (import builtins)
  (assert-all-in ["eval" "AssertionError"]
                 (-> "builtins"
                   (Candidate (Namespace :locals- (locals)))
                   (.attributes))))

(defn test-candidate-attributes-nested []
  (assert-all-in ["--str--" "--call--"]
                 (-> "print.--call--" Candidate (.attributes))))

;; ** Namespacing

(defn test-candidate-namespace-globals []
  (import itertools)
  (assert-in "from-iterable"
             (-> "itertools.chain"
               (Candidate (Namespace :locals- (locals)))
               (.attributes))))

(defn test-candidate-namespace-locals []
  (defclass AClass [])
  (assert (none?
            (-> "AClass"
              Candidate
              (.attributes))))
  (assert (none?
            (-> "AClass"
              (Candidate (Namespace :globals- (globals)))
              (.attributes))))
  (assert-in "--doc--"
             (-> "AClass"
               (Candidate (Namespace :locals- (locals)))
               (.attributes)))

  (setv doesnt-exist 1)
  (assert (-> "doesnt-exist"
            (Candidate (Namespace :locals- (locals)))
            (.evaled?))))

;; ** Annotations

(defn test-annotate-builtin-or-function []
  (assert= "<def print>"
           (-> "print" Candidate (.annotate)))
  (assert= "<def first>"
           (-> "first" Candidate (.annotate))))

(defn test-annotate-class []
  (defclass AClass [])
  (assert= "<class AClass>"
           (-> "AClass"
             (Candidate (Namespace :locals- (locals)))
             (.annotate))))

(defn test-annotate-module-and-aliases []
  (import itertools)
  (assert= "<module itertools>"
           (-> "itertools"
             (Candidate (Namespace :locals- (locals)))
             (.annotate)))

  (import [itertools :as it])
  (assert= "<module it>"
           (-> "it"
             (Candidate (Namespace :locals- (locals)))
             (.annotate))))


(defn test-annotate-vars []
  (setv doesnt-exist False)
  (assert= "<instance doesnt-exist>"
           (-> "doesnt-exist"
             (Candidate (Namespace :locals- (locals)))
             (.annotate))))


(defn test-annotate-compiler []
  (assert= "<compiler try>"
           (-> "try" Candidate (.annotate))))


(defn test-annotate-shadow []
  (assert= "<shadowed is>"
           (-> "is" Candidate (.annotate)))
  (assert= "<shadowed get>"
           (-> "get" Candidate (.annotate))))


(defn test-annotate-macro []
  (assert= "<macro ->>"
           (-> "->" Candidate (.annotate)))
  (assert= "<macro as->>"
           (-> "as->" Candidate (.annotate))))
