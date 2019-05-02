"Expose jedhy's `API` for IDE and metaprogramming use-cases."

;; * Imports

(require [jedhy.macros [*]])
(import [jedhy.macros [*]])

(import [jedhy.inspection [Inspect]]
        [jedhy.models [Candidate
                       Namespace
                       Prefix]])

;; * API

(defclass API [object]
  (defn --init-- [self &optional globals- locals-]
    (self.set-namespace globals- locals-))

  (defn set-namespace [self &optional globals- locals- macros-]
    "Rebuild namespace for possibly given `globals-`, `locals-`, and `macros-`.

Typically, the values passed are:
  globals- -> (globals)
  locals-  -> (locals)
  macros-  -> --macros--"
    (setv self.namespace (Namespace globals- locals- macros-)))

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
