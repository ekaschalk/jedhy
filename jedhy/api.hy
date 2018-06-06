"Expose jedhy's `API` for IDE and metaprogramming use-cases."

(require [jedhy.macros [*]])
(import [jedhy.macros [*]])
(require [hy.extra.anaphoric [*]])
(import
  [jedhy.inspection [Inspect]]
  [jedhy.models [Candidate Namespace Prefix]])


;; * Actions

(defclass API [object]
  (defn --init-- [self &optional globals- locals-]
    (self.set-namespace globals- locals-))

  (defn set-namespace [self &optional globals- locals-]
    "Rebuild namespace according to possibly given `globals-` and `locals-`."
    (setv self.namespace
          (Namespace globals- locals-)))

  (defn complete [self prefix-str]
    "Completions for a prefix string."
    (-> prefix-str
      (Prefix :namespace self.namespace)
      (.complete)))

  (defn annotate [self candidate-str]
    "Annotate a candidate string."
    (-> candidate-str
      (Candidate :namespace self.namespace)
      (.annotate)))

  (defn docs [self candidate-str]
    "Docstring for a candidate string."
    (-> candidate-str
      (Candidate :namespace self.namespace)
      (.get-obj)
      Inspect
      (.docs))))
