(require [src.utils.macros [*]])
(require [hy.extra.anaphoric [*]])
(require [tests.hytest [*]])
(import [tests.hytest [*]])

(import
  pytest

  [src.models [Candidate Prefix]])

;; (import [src.inspection [Parameter]])
;; (import [src.actions [Actions]])
;; (import [src.completion [*]])
;; (import [src.utils.docstrings [*]])

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

(defn test-candidate-compiler []
  (setv compiler?
        (fn-> Candidate (.compiler?)))

  (assert (->> ["try" "for*" "+=" "require"]
             (map compiler?)
             all))
  (assert (->> "doesn't exist"
             compiler?
             none?)))

(defn test-candidate-shadow []
  (setv shadow?
        (fn-> Candidate (.shadow?)))

  (assert (->> ["get" "is" "is_not"]
             (map shadow?)
             all))
  (assert (->> "doesn't exist"
             shadow?
             none?)))
