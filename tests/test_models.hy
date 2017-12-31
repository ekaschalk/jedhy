(require [src.utils.macros [*]])
(require [hy.extra.anaphoric [*]])
(require [tests.hytest [*]])
(import [tests.hytest [*]])

(import
  [src.models [Candidate Namespace Prefix]])


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

;; ;; TODO Right now Candidate, Completer, and Prefix all need namespace/local
;; ;; Either turn into a global datastructure or refactor

;; (defn test-stuff []
;;   (setv complete
;;         (Completer))

;;   ;; print isn't in there
;;   ;; modules aren't in there
;;   ;; imported macros (fn->) *are* in there
;;   ;; (print complete.candidates)

;;   ;; (print (complete (Prefix "print._")))
;;   )

;; ;; (defn test-candidates-globals []
;; ;;   (assert-all-in ["first" "in" "+" "even?"]
;; ;;                  (--HYCOMPANY-get-globals)))

;; ;; (defn test-candidates-trimmed []
;; ;;   (setv candidates
;; ;;         (--HYCOMPANY-get-globals))

;; ;;   (assert-all-in ["first" "even?"] candidates)

;; ;;   (setv trimmed-candidates
;; ;;         (--HYCOMPANY-trim-candidates candidates "fi"))

;; ;;   (assert-in "first" trimmed-candidates)
;; ;;   (assert-not-in "even?" trimmed-candidates))

;; ;; (defn test-candidates-formatted []
;; ;;   (assert-in "builtins"
;; ;;              (--HYCOMPANY "built"))
;; ;;   (assert-in "builtins.eval"
;; ;;              (--HYCOMPANY "builtins.e"))
;; ;;   (assert-in "builtins.eval.--call--"
;; ;;              (--HYCOMPANY "builtins.eval.")))


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
