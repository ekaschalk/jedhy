"Expose jedhy `Actions` for IDE and metaprogramming use-cases."

(require [src.utils.macros [*]])
(import [src.utils.macros [*]])
(require [hy.extra.anaphoric [*]])
(import
  [src.annotations [annotate]]
  [src.completion [Completer]]
  [src.inspection [Inspect]]
  [src.models [Candidate Prefix]])

(defclass Actions [object]
  (defn --init-- [self]
    "Instantiate a Completer with globals set."
    (setv self.completer (Completer)))

  (defn reset-completer [self]
    "Completer reflects changes in globals and imported modules."
    (.reset self.completer))

  (defn complete [self prefix-str]
    "Completions for a prefix string."
    (-> prefix-str
      Prefix
      self.completer))

  (defn annotate [self candidate-str]
    "Annotate a candidate string."
    (-> candidate-str
      Candidate
      annotate))

  (defn docs [self candidate-str]
    "Docstring for a candidate string."
    (-> candidate-str
      Candidate
      (.get-obj)
      Inspect
      (.docs))))
